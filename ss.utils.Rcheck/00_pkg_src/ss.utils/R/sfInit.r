function (parallel = NULL, cpus = NULL, type = NULL, socketHosts = NULL,
    restore = NULL, slaveOutfile = NULL, nostart = FALSE, useRscript = FALSE)
{
    reconnect <- FALSE
    if (nostart)
        return(TRUE)
    if (length(.sfOption) == 0) {
        debug("Setup sfOption...")
        setOption("parallel", FALSE)
        setOption("session", NULL)
        setOption("priority", 1)
        setOption("nodes", 1)
        setOption("stopped", FALSE)
        setOption("init", FALSE)
        data("config", package = "snowfall")
        configM <- as.matrix(t(config))
        config <- as.list(configM)
        names(config) <- dimnames(configM)[[2]]
        if (.sfPresetCPUs > 0)
            setOption("MAXNODES", .sfPresetCPUs)
        else setOption("MAXNODES", as.numeric(config[["MAXNODES"]]))
        setOption("LOCKFILE", "")
        if (as.character(config[["TMPDIR"]]) != "-")
            setOption("TMPDIR", path.expand(as.character(config[["TMPDIR"]])))
        else {
            if (.Platform$OS.type == "unix")
                setOption("TMPDIR", file.path(Sys.getenv("R_SESSION_TMPDIR"),
                  "sfCluster"))
            else setOption("TMPDIR", "")
        }
        setOption("RESTOREFILES", NULL)
        setOption("RESTOREUPDATE", 5)
        setOption("RESTORE", FALSE)
        setOption("CURRENT", NULL)
        setOption("type", "SOCK")
        setOption("sockHosts", NULL)
        if (as.character(config[["RESTDIR"]]) != "-")
            setOption("RESTDIR", path.expand(as.character(config[["RESTDIR"]])))
        else setOption("RESTDIR", file.path(Sys.getenv("HOME"),
            ".sfCluster", "restore"))
        rm(config, pos = globalenv())
    }
    else {
        reconnect <- TRUE
        if (.sfOption$stopped && !.sfOption$init)
            debug("Irregluar init state (error on previous init)...")
        if (!.sfOption$stopped && .sfOption$init) {
            message("Explicit sfStop() is missing: stop now.")
            sfStop()
        }
    }
    searchCommandline(parallel, cpus = cpus, type = type, socketHosts = socketHosts,
        restore = restore)
    if (getOption("verbose") && !reconnect)
        print(.sfOption)
    if (!file.exists(.sfOption$RESTDIR)) {
        dirCreateStop(.sfOption$RESTDIR)
    }
    if (.sfOption$parallel && !nostart) {
        if (startedWithSfCluster() && is.null(.sfOption$session))
            stop("No session-ID but parallel run with sfCluster (something went wrong here?)...")
        if (is.null(.sfOption$nodes) || is.na(as.numeric(.sfOption$nodes)))
            setOption("nodes", 2)
        else setOption("nodes", as.numeric(.sfOption$nodes))
        libList <- list(PVM = "rpvm", MPI = "Rmpi", NWS = "nws",
            SOCK = "")
        if (libList[[.sfOption$type]] != "") {
            if (!require(libList[[.sfOption$type]], character.only = TRUE)) {
                message(paste("Failed to load required library:",
                  libList[[.sfOption$type]], "for parallel mode",
                  .sfOption$type, "\nFallback to sequential execution"))
                return(sfInit(parallel = FALSE))
            }
            else message(paste("Library", libList[[.sfOption$type]],
                "loaded."))
        }
        if (!require(snow)) {
            message(paste("Failed to load library 'snow' required for parallel mode.\n",
                "Switching to sequential mode (1 cpu only)!."))
            return(sfInit(parallel = FALSE))
        }
        if (startedWithSfCluster()) {
            tmp <- file.path(.sfOption$TMPDIR, paste("rout_",
                .sfOption$session, sep = ""))
            if (!reconnect)
                dirCreateStop(.sfOption$TMPDIR)
        }
        else tmp <- ifelse(is.null(slaveOutfile), "/dev/null",
            slaveOutfile)
        setDefaultClusterOptions(type = .sfOption$type)
        setDefaultClusterOptions(homogenous = FALSE)
        if (.sfOption$type == "SOCK") {
            if (is.null(.sfOption$sockHosts) || (length(.sfOption$sockHosts) ==
                0))
                setOption("sockHosts", c(rep("localhost", .sfOption$nodes)))
            else setOption("nodes", length(.sfOption$sockHosts))
            setOption("cluster", try(makeCluster(.sfOption$sockHosts,
                type = "SOCK", outfile = tmp, homogenous = TRUE)))
        }
        else if (.sfOption$type == "PVM") {
            setOption("cluster", try(makeCluster(.sfOption$nodes,
                outfile = tmp)))
        }
        else if (.sfOption$type == "NWS") {
            if (is.null(.sfOption$sockHosts) || (length(.sfOption$sockHosts) ==
                0))
                setOption("sockHosts", c(rep("localhost", .sfOption$nodes)))
            else setOption("nodes", length(.sfOption$sockHosts))
            setOption("cluster", try(makeNWScluster(.sfOption$sockHosts[1:.sfOption$nodes],
                type = "NWS", outfile = tmp)))
        }
        else {
#            setOption("cluster", try(makeMPIcluster(.sfOption$nodes,
#                outfile = tmp, homogenous = TRUE, useRscript = useRscript)))
            setOption("cluster", try(getMPIcluster()))
        }
        if (is.null(.sfOption$cluster) || inherits(.sfOption$cluster,
            "try-error"))
            stop(paste("Starting of snow cluster failed!", geterrmessage(),
                .sfOption$cluster))
        setOption("init", TRUE)
        setOption("stopped", FALSE)
        if (!reconnect) {
            if (!is.null(.sfOption$LOCKFILE) && file.exists(.sfOption$LOCKFILE)) {
                if (unlink(.sfOption$LOCKFILE) != 0)
                  warning("Unable to remove startup lockfile: ",
                    .sfOption$LOCKFILE)
                else message("Startup Lockfile removed: ", .sfOption$LOCKFILE)
            }
            if (getOption("verbose")) {
                if (tmp == "/dev/null")
                  message("Slave output suppressed. Use 'slaveOutfile' to activate.")
                else message(paste("Temporary log for STDOUT/STDERR (on each node): ",
                  tmp, "\n", "Cluster started with", .sfOption$nodes,
                  "CPUs.", "\n"))
            }
            else debug(paste("Temporary log for STDOUT/STDERR (on each node): ",
                tmp, "\n", "Cluster started with", .sfOption$nodes,
                "CPUs.", "\n"))
            .startInfo <- strsplit(Sys.info(), "\n")
            .startMsg <- paste(sep = "", "JOB STARTED AT ", date(),
                " ON ", .startInfo$nodename, " (OS", .startInfo$sysname,
                ") ", .startInfo$release, "\n")
            sfExport(".sfOption", ".startMsg", local = TRUE,
                namespace = "snowfall", debug = DEBUG)
            sfCat(.startMsg, "\n", master = FALSE)
            sfCat(paste("R Version: ", R.version$version.string,
                "\n\n"))
            sfRemove(".startMsg")
        }
        else sfExport(".sfOption", local = FALSE, namespace = "snowfall")
    }
    else {
        setOption("init", TRUE)
        setOption("stopped", FALSE)
        setOption("cluster", NULL)
    }
    if (sfParallel()) {
        message(paste("snowfall ", packageDescription("snowfall")$Version,
            " initialized (using snow ", packageDescription("snow")$Version,
            "): parallel execution on ", sfCpus(), " CPUs.\n",
            sep = ""))
    }
    else {
        message(paste("snowfall", packageDescription("snowfall")$Version,
            "initialized: sequential execution, one CPU.\n"))
    }
    return(invisible(TRUE))
}
