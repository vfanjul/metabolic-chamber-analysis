time0 = proc.time() # Runtime marker

###---- 1. Set parameters ----
processnew = F # Process only new experiments (T) or all experiments (F)
exdying = T # Exclude dying animals (default is T)
switchtime = 2 # Switching time in experiments
torporlim = 80 # If EE drops under this threshold, we consider the animal is in torpor

## File location
if (.Platform$OS.type == "unix") setwd("/Volumes/Victor/") else setwd("S:/LAB_VA/LAB/Victor/") # Set base directory
baseroute = "Heart progerin project/Raw data/Metabolic.cages/" # File route
route = paste0(baseroute, "Raw xls/") # xls folder


###---- 2. Libraries & functions ----
packages = c("data.table", "matrixStats", "zoo", "readxl", "beeswarm")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) install.packages(setdiff(packages, rownames(installed.packages())))  
for (i in packages) library(i, character.only = T)

datefun = function (x) {
  if (any(grepl("/",x))) {
    date1 = sub("/.*", "", x)
    date2 = sub("/.*", "", sub("[[:digit:]]*/", "", x))
    date3 = sub(".*/", "", x)
  } else if (any(grepl("-",x))) {
    date1 = sub("-.*", "", x)
    date2 = sub("-.*", "", sub("[[:digit:]]*/", "", x))
    date3 = sub(".*-", "", x)
  }
  if (max(nchar(date3)) == 4) {
    if (max(as.numeric(date2), na.rm = T) > 12 & max(as.numeric(date1), na.rm = T) <= 12) {
      as.Date(paste0(date3, "/", date1, "/", date2))
    } else if (max(as.numeric(date1), na.rm = T) > 12 & max(as.numeric(date2)) <= 12){
      as.Date(paste0(date3, "/", date2, "/", date1))
    } else as.Date(paste0(date3, "/", date2, "/", date1)) # Might mistake days for months
  } else if (max(nchar(date1)) == 4) {
    as.Date(x)
  } else if (max(as.numeric(date1), na.rm = T) > 31 | max(as.numeric(date3), na.rm = T) > as.integer(format(Sys.Date(),"%Y"))-2000) {
    as.Date(paste0(20,x))
  } else if (max(as.numeric(date3), na.rm = T) > 31 | max(as.numeric(date1), na.rm = T) > as.integer(format(Sys.Date(),"%Y"))-2000) {
    if (max(as.numeric(date2), na.rm = T) > 12 & max(as.numeric(date1), na.rm = T) <= 12) {
      as.Date(paste0(20, date3, "/", date1, "/", date2))
    } else if (max(as.numeric(date1), na.rm = T) > 12 & max(as.numeric(date2), na.rm = T) <= 12){
      as.Date(paste0(20, date3, "/", date2, "/", date1))
    } else as.Date(paste0(20, date3, "/", date2, "/", date1)) # Might mistake days for months
  } else if (max(as.numeric(date2), na.rm = T) > 12) {
    as.Date(paste0(20, date3, "/", date1, "/", date2))
  } else as.Date(paste0(20, date3, "/", date2, "/", date1)) # Might mistake between days, months and years
}


###---- 3. Import & arrange data ----
## Select experiments & files
previous = c()
if (file.exists(paste0(route, "processed.log.txt")) & processnew) previous = data.frame(fread(paste0(route, "processed.log.txt")))[,1]
experiments = setdiff(list.dirs(route, recursive = F, full.names = F), previous)


## Initialize data frames and iterate
processed = c()
info = c()
sheets = c("o","a","i")
for (sh in sheets) assign(paste0("data_",sh), c())

for (exp in experiments) {
  sessions = dir(paste0(route, exp), pattern = ".xls")
  for (ses in sessions) {
    tryCatch({ # Bypass errors

      ## Arrange info for each session and experiment
      excelinfo = read_xls(paste0(route, exp, "/", ses), sheet = 1, skip = 11, col_names = F, n_max = 10, .name_repair = "minimal", na = c("", "N/A", "NA", "nan", "NaN"), progress = F)
      excelinfo = data.frame(t(excelinfo), stringsAsFactors = F)
      names(excelinfo) = gsub("Code", "Id", gsub("Weight", "Weight (g)", gsub(":", "", excelinfo[1,])))
      excelinfo = excelinfo[!(excelinfo$Cage == "Cage: " | excelinfo$Cage == "Cage:") & !is.na(excelinfo$Cage),]
      
      excelinfo[,"Weight (g)"] = as.numeric(gsub(",", ".", gsub(" g", "", excelinfo[,"Weight (g)"])))
      excelinfo = cbind.data.frame("Experiment" = exp, "Module" = paste(nrow(excelinfo), "cages"), excelinfo, stringsAsFactors = F)
      info = rbind.data.frame(info, excelinfo) # Append session info to general info data frame

      ## Arrange data for each sheet, session and experiment
      for (sh in sheets) {
        exceldata = read_xls(paste0(route, exp, "/", ses), sheet = which(sheets == sh), skip = 21, .name_repair = "minimal", na = c("", "N/A", "NA", "nan", "NaN"), progress = F)
        cols = length(unique(names(exceldata)))
        firstcols = seq(1, ncol(exceldata), cols)
        allcages = c()
        for (firstcol in firstcols) {
          eachcage = cbind.data.frame("Id" = excelinfo$Id[as.numeric(gsub("Cage ", "", excelinfo$Cage)) == which(firstcols == firstcol)], 
                                      "Date" = as.Date(exp), 
                                      "Weight (g)" = excelinfo[as.numeric(gsub("Cage ", "", excelinfo$Cage)) == which(firstcols == firstcol), "Weight (g)"], 
                                      exceldata[,firstcol:(firstcol + cols - 1)], stringsAsFactors = F)
          names(eachcage) = gsub(",", ".", gsub("Relative time", "Time", names(eachcage)))
          eachcage$Time = as.numeric(eachcage$Time)
          eachcage = eachcage[!is.na(eachcage$`Absolute time`),]
          if (min(as.Date(eachcage$`Absolute time`)) < as.Date(exp)) eachcage$`Absolute time` = eachcage$`Absolute time` + difftime(as.Date(exp), as.Date(min(eachcage$`Absolute time`))) # Correct start date
          if (sh == "i") { # Calculate non cumulative food/drink consumption per min in session
            eachcage$`Food intake (g/min)` = (eachcage$`Consumption (g)` - c(0, eachcage$`Consumption (g)`[-length(eachcage$`Consumption (g)`)]))/switchtime 
            eachcage$`Water intake (ml/min)` = (eachcage$`Consumption (ml)` - c(0, eachcage$`Consumption (ml)`[-length(eachcage$`Consumption (ml)`)]))/switchtime
            eachcage$`Food intake (g/min)`[eachcage$`Food intake (g/min)` < 0] = 0
            eachcage$`Water intake (ml/min)`[eachcage$`Water intake (ml/min)` < 0] = 0
            eachcage = eachcage[,-6:-11]
          }
          allcages = rbind.data.frame(allcages,eachcage[-nrow(eachcage),])
        }
        
        allcages = allcages[!is.na(allcages$`Absolute time`),]
        allcages[is.na(allcages)] = 0
        assign(paste0("data_", sh), rbind.data.frame(get(paste0("data_", sh)), allcages)) # Append session sheet data to general sheet data frame
      }
    }, error = function (e) cat("ERROR: experiment ", exp, ", session ", ses, ". ", conditionMessage(e), "\n")) # Error message
  }
  fwrite(cbind.data.frame(exp, Sys.time()), file = paste0(route, "processed.log.txt"), row.names = F, col.names = F, sep = "\t", append = T, eol = "\r\n") # Save processed experiments log
  setTxtProgressBar(txtProgressBar(style = 3), which(experiments == exp)/length(experiments)) # Progress bar
}
rm(exceldata, excelinfo, allcages, eachcage) # Remove accessory objects


###---- 4. Consolidate data frames ----
## Consolidate info data set
info = info[with(info, order(Experiment, as.numeric(gsub(" cages", "", Module)), as.numeric(gsub("Cage ", "", Cage)))),]
info = info[!duplicated(info[,c("Id", "Experiment")]),]

## Reset relative time in each experiment
for (Date in unique(info$Experiment)) {
  starttime = as.POSIXct(paste(as.Date(min(data_o[data_o$Date == Date, "Absolute time"])), "05:00:00"), "UTC")
  data_o$Time[data_o$Date == Date] = round(as.numeric(difftime(data_o[data_o$Date == Date, "Absolute time"], starttime, units = "min"))/switchtime, 0)*switchtime
  data_a$Time[data_a$Date == Date] = round(as.numeric(difftime(data_a[data_a$Date == Date, "Absolute time"], starttime, units = "min"))/switchtime, 0)*switchtime
  data_i$Time[data_i$Date == Date] = round(as.numeric(difftime(data_i[data_i$Date == Date, "Absolute time"], starttime, units = "min"))/switchtime, 0)*switchtime
}

## Consolidate oximetry
data_o = data_o[!duplicated(data_o[,c("Id", "Date", "Time")]),]
names(data_o) = gsub("0,", "0.", gsub("Kcal", "kcal", names(data_o)))
data_o$`VO2 (ml/min/kg)` = data_o$`VO2 (ml/min)`/(data_o$`Weight (g)`/1000)
data_o$`VCO2 (ml/min/kg)` = data_o$`VCO2 (ml/min)`/(data_o$`Weight (g)`/1000)
data_o$`EE (kcal/day/kg)` = data_o$`EE (kcal/day)`/(data_o$`Weight (g)`/1000)

## Consolidate activity
data_a = data_a[!duplicated(data_a[,c("Id", "Date", "Time")]),]
data_a[,c("Activity", "Rearing")] = data_a[,c("Activity", "Rearing")]/switchtime
data_a$Activity[data_a$Activity < 0] = 0
data_a$Rearing[data_a$Rearing < 0] = 0
data_a$`Total activity (counts/min)` = data_a$Activity + data_a$Rearing
names(data_a)[names(data_a) == "Activity"] = "Activity (counts/min)"
names(data_a)[names(data_a) == "Rearing"] = "Rearing (counts/min)"

## Consolidate intake
data_i = data_i[!duplicated(data_i[,c("Id", "Date", "Time")]),]
data_i = data_i[with(data_i, order(Date, Id, Time)),]
for (Date in unique(data_i$Date)) for (Id in unique(data_i$Id[data_i$Date == Date])) {
  filter = data_i$Date == Date & data_i$Id == Id
  data_i[filter,"Food consumption (g)"] = cumsum((data_i[filter,"Food intake (g/min)"])*switchtime)
  data_i[filter,"Water consumption (ml)"] = cumsum((data_i[filter,"Water intake (ml/min)"])*switchtime)
}
data_i$`Food intake (g/min/kg)` = data_i$`Food intake (g/min)`/(data_i$`Weight (g)`/1000)
data_i$`Water intake (ml/min/kg)` = data_i$`Water intake (ml/min)`/(data_i$`Weight (g)`/1000)
data_i$`Food consumption (g/kg)` = data_i$`Food consumption (g)`/(data_i$`Weight (g)`/1000)
data_i$`Water consumption (ml/kg)` = data_i$`Water consumption (ml)`/(data_i$`Weight (g)`/1000)
data_i$`Total intake (g/min)` = data_i$`Food intake (g/min)` + data_i$`Water intake (ml/min)`
data_i$`Total intake (g/min/kg)` = data_i$`Food intake (g/min/kg)` + data_i$`Water intake (ml/min/kg)`


## Merge data frames
rawdata = merge(data_o[,-4], data_a, all = T, sort = F)
rawdata = merge(rawdata, data_i, all = T, sort = F)
rawdata = rawdata[with(rawdata, order(Date, Id, Time)),] # Reorder
rawdata$`Absolute time`[is.na(rawdata$`Absolute time`)] = data_o$`Absolute time`[data_o$Time %in% rawdata$Time[is.na(rawdata$`Absolute time`)]] # Fill possible NAs in Absolute time
names(rawdata)[names(rawdata) == "Time"] = "Time (min)"


## Determine light cycle, torpor bouts and events
rawdata$Cycle = "Dark"
rawdata$Cycle[5 <= hour(rawdata$`Absolute time`) & 17 > hour(rawdata$`Absolute time`)] = "Light"

rawdata$Torpor = NA
filter = !is.na(rawdata$`EE (kcal/day/kg^0.75)`)
rawdata[filter,"Torpor"] = 0
rawdata[filter & rawdata$`EE (kcal/day/kg^0.75)` < torporlim,"Torpor"] = 1 # Torpor state
filter2 = c(rawdata[filter,"Torpor"][-1], 0) == c(0,rawdata[filter,"Torpor"][-nrow(rawdata[filter,])])
rawdata[filter,"Torpor"][filter2] = c(rawdata[filter,"Torpor"][-1], 0)[filter2] # Correct torpor state based on surroundings

for (Date in unique(rawdata$Date)) for (Id in unique(rawdata$Id[rawdata$Date == Date])) {
  filter = rawdata$Date == Date & rawdata$Id == Id
  rawdata[filter,"Torpor"] = as.numeric(na.locf(rawdata[filter,"Torpor"], na.rm = F, fromLast = T)) # Impute torpor state
}
rawdata$Torpor = as.numeric(na.locf(rawdata$Torpor, na.rm = F, fromLast = F)) # Impute remaining NAs

rawdata$Event = ""
rawdata[rawdata$Torpor == 1 & c(rawdata$Torpor[-1],0) == 0, "Event"] = "Te" # Torpor bout end
rawdata[rawdata$Torpor == 1 & c(0,rawdata$Torpor[-nrow(rawdata)]) == 0, "Event"] = "T0" # Torpor bout start

## Determine if death
rawdata[!is.na(rawdata$`VO2 (ml/min/kg^0.75)`) & (rawdata$`VO2 (ml/min/kg^0.75)` <= 0 | rawdata$`VCO2 (ml/min/kg^0.75)` <= 1), "Event"] = "Death" # Animal is dead
filter = (!is.na(rawdata$`VO2 (ml/min/kg^0.75)`) & !grepl("T.", rawdata$Event))
for (i in 1:4) rawdata[filter,"Event"][rawdata[filter,"Event"] != c(rawdata[filter,"Event"][-1:-i], rep("", i))] = "" # Check death is not artifact
filter = rawdata$Event == "Death"
rawdata[filter, "Event"][rawdata[filter,"Id"] == c(0,rawdata[filter,"Id"][-nrow(rawdata[filter,])])] = "" # Indicate only moment of death



###---- 5. Clean data ----
censornames = paste("Censored", c("oximetry","activity","rearing","food","drink"))

## Import censored info and merge with rawdata
if (any(file.exists(sapply(c("txt", "xls", "xlsx"), function (x) paste0(baseroute, "censored.", x))))) { # Ommit if no censored info
  if (file.exists(paste0(baseroute, "censored.txt"))) {
    censored = data.frame(fread(paste0(baseroute, "censored old.txt"), na = c("", "N/A", "NA", "nan", "NaN")))[,1:9]
  } else censored = read_excel(dir(baseroute, pattern = "^censored\\.", full.names = T), sheet = 1, na = c("", "N/A", "NA", "nan", "NaN"), progress = F)[,1:9]
  names(censored) = c("Id","Date","Time min","Time max", censornames)
  censored$Date = datefun(censored$Date)
  
  rawdata = merge(rawdata, censored, all = T, sort = F) # Merge
  rawdata = rawdata[with(rawdata, order(Date, Id,`Time (min)`)),] # Reorder
  rawdata[!is.na(rawdata$`Time min`) & rawdata$`Time min` > rawdata$`Time (min)`, censornames] = 0 # Correct censored by time range
  rawdata[!is.na(rawdata$`Time max`) & rawdata$`Time max` < rawdata$`Time (min)`, censornames] = 0
  rawdata[is.na(rawdata$`Censored oximetry`), censornames] = 0
  rawdata = rawdata[,!names(rawdata) %in% c("Time min", "Time max")]
} else rawdata[,censornames] = 0

## Censurate torpor and events
rawdata$Torpor[rawdata$`Censored oximetry` == 1] = 0 # Reset torpor state in censored
rawdata$Event[rawdata$`Censored oximetry` == 1] = "" # Reset event in censored
for (Date in unique(rawdata$Date)) { # Censor dead
  for (Id in unique(rawdata$Id[rawdata$Date == Date & rawdata$Event == "Death"])) {
    filter = rawdata$Date == Date & rawdata$Id == Id
    rawdata[filter & rawdata$`Time (min)` >= rawdata$`Time (min)`[filter & rawdata$Event == "Death"], censornames] = 1
  }
}


## Exclude censored data
ncrawdata = rawdata
if (exdying) for (Date in unique(ncrawdata$Date)) { # Exclude data from dying animals
  for (Id in unique(ncrawdata$Id[ncrawdata$Date == Date & ncrawdata$Event == "Death"])) {
    ncrawdata[ncrawdata$Date == Date & ncrawdata$Id == Id, censornames] = 1
  }
}

names_o = setdiff(names(data_o), names(data_a))
names_a = setdiff(names(data_a), names(data_i))
names_i = setdiff(names(data_i), names(data_a))
ncrawdata[ncrawdata$`Censored oximetry` == 1, names_o] = NA
ncrawdata[ncrawdata$`Censored activity` == 1, grep("Rearing", names_a, value = T, invert = T)] = NA
ncrawdata[ncrawdata$`Censored rearing` == 1, grep("Activity", names_a, value = T, invert = T)] = NA
ncrawdata[ncrawdata$`Censored food` == 1, grep("Water", names_i, value = T, invert = T)] = NA
ncrawdata[ncrawdata$`Censored drink` == 1, grep("Food", names_i, value = T, invert = T)] = NA

ncrawdata = ncrawdata[rowSums(ncrawdata[,censornames]) != length(censornames), -grep("ensored", names(ncrawdata))] # Discard empty rows


## Calculate mean data
names_par = c(names_o, names_a, names_i)

meandata = c()
for (Date in as.character(unique(ncrawdata$Date))) for (Id in unique(ncrawdata$Id[ncrawdata$Date == Date])) {
  for (Cycle in unique(ncrawdata$Cycle)) for (Torpor in unique(ncrawdata$Torpor[ncrawdata$Date == Date & ncrawdata$Id == Id & ncrawdata$Cycle == Cycle])) {
    filter = ncrawdata$Date == Date & ncrawdata$Id == Id & ncrawdata$Cycle == Cycle & ncrawdata$Torpor == Torpor
    Weight = unique(ncrawdata$`Weight (g)`[ncrawdata$Date == Date & ncrawdata$Id == Id])
    meandata = rbind.data.frame(meandata, cbind.data.frame(Id, Date, Cycle, Torpor, 
                                                           "Weight (g)" = Weight, "Counts" = nrow(ncrawdata[filter,]),
                                                           t(colMeans(ncrawdata[filter, names_par], na.rm = T))))
  }
}



###---- 6. Plot and export data ----
## Create directories
dir.create(paste0(baseroute, "Plots"), showWarnings = F)
dir.create(paste0(baseroute, "Plots/Plots per subject"), showWarnings = F)
dir.create(paste0(baseroute, "Plots/EE comparison per experiment"), showWarnings = F)
dir.create(paste0(baseroute, "Plots/Activity comparison per experiment"), showWarnings = F)
dir.create(paste0(baseroute, "Plots/Intake comparison per experiment"), showWarnings = F)
dir.create(paste0(baseroute, "Plots/Plots per parameter"), showWarnings = F)

## Parameter plots per subject
ploty = c("V(C)O2 (ml/min/kg^0.75)", "EE (kcal/day/kg^0.75)", "RQ", "Activity (counts/min)", "Intake (g/min/kg)") # Y labels
plotlim = c(40, 260, 1.2, 20, 20) # Max y limits
plotparam = list(c("VO2 (ml/min/kg^0.75)", "VCO2 (ml/min/kg^0.75)"), "EE (kcal/day/kg^0.75)", "RQ", c("Activity (counts/min)", "Rearing (counts/min)"), c("Food intake (g/min/kg)", "Water intake (ml/min/kg)")) # Parameters
censcols = list("Censored oximetry", "Censored oximetry", "Censored oximetry", c("Censored activity", "Censored rearing"), c("Censored food", "Censored drink")) # Censored data

for (Date in as.character(unique(rawdata$Date))) {
  for (Id in unique(rawdata$Id[rawdata$Date == Date])) {
    pos = info[info$Experiment == Date & info$Id == Id, c("Module", "Cage")] # Module and cage
    filter = rawdata$Id == Id & rawdata$Date == Date
    daysplot = ceiling(max(rawdata$`Time (min)`[filter])/60/24) # X max limit (number of complete days)
    pdf(paste0(baseroute, "Plots/Plots per subject/", Date, " ", paste(gsub(" cages", "", gsub("Cage ", "", pos)), collapse = " "), " ", Id, " metabolic chamber.pdf"), width = 11.7, height = 8.3, pointsize = 1) # Save plot
    par(mfrow = c(5, 1), bty = "l", cex.axis = 1.3, cex.lab = 1.3, pch = 19, cex = 1, lwd = 0.1, mar = c(0, 5, 2, 1), oma = c(5,1,4,1)) # Graphical parameters
    for (i in 1:5) { # Subplots for each group of parameters
      plot(NULL, type = "l", xlab = "", ylab = ploty[i], xaxs = "i", yaxs = "i", xaxt = "n", ylim = c(0,plotlim[i]), xlim = c(0, daysplot*60*24), xaxp = c(0, daysplot*60*24, 120)) # Initialize plot
      if (i == 1) mtext(side = 3, line = 2, at = 500, paste0(Date," ", Id, " (", paste(pos, collapse = ", "), ")")) # Plot info (Date, Id, module, cage)
      for (j in 0:(daysplot-1)) polygon(c(12*60,12*60,24*60,24*60) + j*24*60, c(0,plotlim[i],plotlim[i],0), col = adjustcolor(1, alpha.f = 0.2), border = NA) # Light and dark cycles
      abline(v = rawdata[filter & rawdata$Event == "T0","Time (min)"], col = 1, lty = 2, lwd = 1) # Topor start
      abline(v = rawdata[filter & rawdata$Event == "Te","Time (min)"], col = 1, lty = 3, lwd = 1) # Torpor end
      for (j in plotparam[[i]]) lines(rawdata[filter & !is.na(rawdata[,j]),j] ~ rawdata[filter & !is.na(rawdata[,j]),"Time (min)"], col = adjustcolor(which(plotparam[[i]] == j)*2, alpha.f = 1/which(plotparam[[i]] == j)), lwd = 2) # Parameter lines
      
      filter2 = F
      for (j in 1:length(censcols[[i]])) filter2 = filter2 | rawdata[,censcols[[i]][j]] == 1 # Censored filter
      censlim = as.numeric(gsub("NA", 0, paste(summary(rawdata[filter & filter2, "Time (min)"])[c(1,6)]))) # Censored range
      polygon(sort(rep(censlim,2)), c(0,plotlim[i],plotlim[i],0), col = 1, border = NA, density = 10, angle = 90) # Censored area
      legend(daysplot*60*24*.886, y = plotlim[i], cex = 1.2, legend = plotparam[[i]], text.col = c(2,4), bty = "n", y.intersp = 2) # Legend
    }
    abscissa = c(rep(c(seq(6, 23, by = 2), seq(0, 5, by = 2)), daysplot), 6) # X positions
    axis(1, at = seq(0, daysplot*60*24, by = 120),labels = abscissa) # X axis
    mtext(side = 1, line = 3, at = mean(seq(0, daysplot*60*24, by = 120)), cex = 1.5, "Time (h)") # X label
    dev.off() # Close plot
  }
  setTxtProgressBar(txtProgressBar(style = 3), which(as.character(unique(rawdata$Date)) == Date)/length(unique(rawdata$Date))) # Progress bar
}


## EE, activity & intake comparison plots per experiment
expplots = function (plotlim, plotname, ylab, vars, censcols) {
  for (Date in as.character(unique(rawdata$Date))) {
    ncages = nrow(info[info$Experiment == Date,]) # Number of chambers in experiment
    daysplot = ceiling(max(rawdata$`Time (min)`[rawdata$Date == Date])/60/24) # X max limit (number of complete days)
    pdf(paste0(baseroute, "Plots/", plotname, " per experiment/", Date, " ", plotname, ".pdf"), width = 11.7, height = 8.3*ncages/5, pointsize = 1) # Save plot
    par(mfrow = c(ncages, 1), bty = "l", cex.axis = 1.3, cex.lab = 1.3, pch = 19, cex = 1, lwd = 0.1, mar = c(0, 5, 2, 1), oma = c(5,1,4,1)) # Graphical parameters
    for (i in 1:ncages) { # Subplots for each chamber
      Id = info[info$Experiment == Date, "Id"][i]
      pos = info[info$Experiment == Date & info$Id == Id, c("Module", "Cage")] # Module and cage
      filter = rawdata$Id == Id & rawdata$Date == Date
      plot(NULL, type = "l", xlab = "", ylab = ylab, xaxs = "i", yaxs = "i", xaxt = "n", ylim = c(0,plotlim), xlim = c(0, daysplot*60*24), xaxp = c(0, daysplot*60*24, 120)) # Initialize plot
      mtext(side = 3, line = 0, at = 500, paste0(Date," ", Id, " (", paste(pos, collapse = ", "), ")")) # Plot info (Date, Id, module, cage)
      for (j in 0:(daysplot-1)) polygon(c(12*60,12*60,24*60,24*60) + j*24*60, c(0,plotlim,plotlim,0), col = adjustcolor(1, alpha.f = 0.2), border = NA) # Light and dark cycles
      abline(v = rawdata[filter & rawdata$Event == "T0","Time (min)"], col = 1, lty = 2, lwd = 1) # Topor start
      abline(v = rawdata[filter & rawdata$Event == "Te","Time (min)"], col = 1, lty = 3, lwd = 1) # Torpor end
      for (j in vars) lines(rawdata[filter & !is.na(rawdata[,j]),j] ~ rawdata[filter & !is.na(rawdata[,j]),"Time (min)"], col = adjustcolor(which(vars == j)*2, alpha.f = 1/which(vars == j)), lwd = 2) # Parameter lines
      filter2 = F
      for (j in 1:length(censcols)) filter2 = filter2 | rawdata[,censcols[j]] == 1 # Censored filter
      censlim = as.numeric(gsub("NA", 0, paste(summary(rawdata[filter & filter2, "Time (min)"])[c(1,6)]))) # Censored range
      polygon(sort(rep(censlim,2)), c(0,plotlim,plotlim,0), col = 1, border = NA, density = 10, angle = 90) # Censored area
      legend(daysplot*60*24*.886, y = plotlim, cex = 1.2, legend = vars, text.col = c(2,4), bty = "n", y.intersp = 2) # Legend
    }
    abscissa = c(rep(c(seq(6, 23, by = 2), seq(0, 5, by = 2)), daysplot), 6) # X positions
    axis(1, at = seq(0, daysplot*60*24, by = 120),labels = abscissa) # X axis
    mtext(side = 1, line = 3, at = mean(seq(0, daysplot*60*24, by = 120)), cex = 1.5, "Time (h)") # X label
    dev.off() # Close plot
    setTxtProgressBar(txtProgressBar(style = 3), which(as.character(unique(rawdata$Date)) == Date)/length(unique(rawdata$Date))) # Progress bar
  }
}

expplots(260, "EE comparison", "EE (kcal/day/kg^0.75)", "EE (kcal/day/kg^0.75)", "Censored oximetry")
expplots(20, "Activity comparison", "Activity (counts/min)", c("Activity (counts/min)", "Rearing (counts/min)"), c("Censored activity", "Censored rearing"))
expplots(20, "Intake comparison", "Intake (g/min/kg)", c("Food intake (g/min/kg)", "Water intake (ml/min/kg)"), c("Censored food", "Censored drink"))


## Mean plots per parameter
for(param in names_par) {
  pdf(paste0(baseroute, "Plots/Plots per parameter/", which(names_par == param), " ", gsub("([[:punct:]]| )", ".", param), ".pdf"), 5/0.9, 4/0.9, pointsize = 12, useDingbats = F) # Save plot
  par(bty = "l", cex.axis = 0.75, mar = c(2,3,2,1), mgp = c(1.5,0.25,0), tck = - 0.01) # Graphical parameters
  ylim = summary(meandata[,param])[c(1,6)]*c(0.9,1.1) # Y max limit
  boxplot(0,0,0,0, pch = 20, ylim = ylim, yaxs = "i", xaxt = "n", xlab = "", ylab = param, add = F, at = 1:4) # Initialize plot
  polygon(c(0,0,2.5,2.5), c(ylim, rev(ylim)), col = adjustcolor(1, alpha.f = 0.2), border = NA) # Light and dark cycle
  boxplot(meandata[,param] ~ paste(meandata$Cycle, meandata$Torpor), pch = 20, xaxt = "n", add = T, col = adjustcolor(1, alpha.f = 0), border = 1:2, boxlwd = 2.5, notch = T) # Boxes
  beeswarm(meandata[,param] ~ paste(meandata$Cycle, meandata$Torpor), pch = 20, add = T, col = adjustcolor(1:2, alpha.f = 0.2), corral = "wrap") # Dots
  axis(side = 1, at = 1:4, labels = rep(c("Conscious", "Torpor"),2), tck = 0, lty = 0, mgp = c(3,1.2,0), line = -1) # Torpor tags
  axis(side = 3, at = c(1.5,3.5), labels = c("Dark", "Light"), tck = 0, lty = 0, mgp = c(3,1.2,0), line = -1) # Cycle tags
  dev.off() # Close plot
}



## Export data

newfiles = !processnew
for (i in c("rawdata", "censored&rawdata", "meandata", "info")) newfiles = newfiles | !file.exists(paste0(baseroute, i, ".txt")) # Determine whether to append or rewrite files
fwrite(info, file = paste0(baseroute, "info.txt"), row.names = F, col.names = newfiles, sep = "\t", append = !newfiles)
fwrite(ncrawdata, file = paste0(baseroute, "rawdata.txt"), row.names = F, col.names = newfiles, sep = "\t", append = !newfiles)
fwrite(rawdata, file = paste0(baseroute, "censored&rawdata.txt"), row.names = F, col.names = newfiles, sep = "\t", append = !newfiles)
fwrite(meandata, file = paste0(baseroute, "meandata.txt"), row.names = F, col.names = newfiles, sep = "\t", append = !newfiles)


time1 = proc.time(); as.ITime((time1 - time0)[[3]]) # Runtime
