corePkgs = c("base", "methods", "stats", "graphics", "grDevices", "utils", "datasets")
corePkgSyms  = unlist(lapply(corePkgs, function(pkg) objects(paste0("package:", pkg))))

librarySymbols = function(nm, ..., verbose=FALSE, attach=FALSE)
    {
        deps = getDeps(nm, found = corePkgs , verbose = verbose)
        libs = c(nm, deps)
        libs = gsub("([[:space:]]|\\n)+", "", libs)
        #we load but DON'T attach the library and all its dependencies to resolve its symbols
        allsyms = unlist(lapply(libs, function(x)
            {
                e = loadNamespace(x, partial=TRUE)
                ret =ls(envir= e)
                names(ret) = rep(x, times = length(ret))
                ret
            }))
        allsyms
    }


stripVrDeps = function(txt) {
    
    txt = gsub("[[:space:]]\\(.*", "", txt)
    gsub("[^[:alnum:]_.]", "", txt)
}


getDeps = function(name, verbose=FALSE, found = character())
    {
     
        deps = character()
        if(length(name) > 1)
            {
                for(nm in name)
                    {
                        deps = c(deps, getDeps(nm, verbose = verbose, found = found))
                        found = c(found, deps)
                    }
                return(unique(deps))
            }

                                        #can't pass characters to library's help argument ....
        if(verbose)
            print(sprintf("Determining dependencies of package %s", name))

        if(length(grep("R .*", name)))
            {
                if(verbose)
                    message(sprintf("skipping R version dependency: %s", name))
                return(character())
            }
        cd = sprintf("library(help='%s')", name)
        pinfo = eval(parse(text=cd))$info[[1]]
        dline = grep("^Depends:", pinfo)
        if(!length(dline))
            return(character())
        depline = pinfo[grepl("^Depends:", pinfo)]
        deps = unlist(strsplit(gsub("^Depends:[^[:alpha:]]*", "", depline), split = "\\s*,\\s*"))
        deps = deps[!grepl("R .*", deps)]
        deps = stripVrDeps(deps)
        deps = deps[! (deps %in% found) ]
      
        newdeps = getDeps(deps, verbose=verbose, found = c(deps, found))
        while(length(newdeps))
            {
                deps = c(newdeps, deps)
                newdeps = getDeps(newdeps, verbose = verbose, found = c(deps, found))
            }

        deps = unique(deps)
        deps
        
    }
