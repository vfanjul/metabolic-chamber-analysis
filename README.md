# Metabolic Chamber Analysis

If we are interested in studying the metabolism in rodent animal models, metabolic chambers are a great tool, as they can measure respiratory exchange, intake and physical activity. Nonetheless, these experiments yield large amounts of data that are difficult to analyse for the average user. Moreover, the sofware provided with the equipment is often rudimentary and data export is only possible in a poorly structured way, which complicates the analysis. This is the case of [METABOLISM Software](https://www.panlab.com/en/products/metabolism-software-panlab), provided for [OXYLETPRO - PHYSIOCAGE](https://www.panlab.com/en/products/oxyletpro-home-cage) Panlab systems.

This pipeline simplifies the job of the researcher as it automatizes most of the process. It involves:

- Data structuring
- Variable engineering
- Data cleaning & censoring
- Data visualization
- Data export

In the end we obtain publication ready plots and tabulated data which can be easily explored and imported into a statistical analysis tool.

## Prerequisites
- Having [METABOLISM Software](https://www.panlab.com/en/products/metabolism-software-panlab) installed
- Exporting `.xls` files from METABOLISM
- Organize `.xls` files into `yyyy-mm-dd` folders (named after the date of the experiment)
- Place all date folders into a `raw xls` folder
- Include a `censored` file

## Exporting data from METABOLISM
1. Open raw file
2. Go to the session explorer
3. Select desired session
4. Set the following parameters:
   - Oximetry Report: check
   - Metabolic Equations: complete
   - Body Weight Exponent: 0.75
   - Energy Expenditure Units: Kcal/day
   - Average Report: **uncheck**
   - Activity Report: check
   - Normalization by Animal Weight: **uncheck**
   - Intake Report: check
   - Intake: check
5. Press calculate
6. Save results as Excel file (name is not important)
7. Repeat steps for other sessions and files (discard training or setup sessions)

## Creating censored data file

Some experiments may include artifacts that must be elimintated in order to guarantee a robust analysis. These artifacts may include:
- Missing signal from sensors
- Errors in signal scaling or calibration
- Empty chambers or dying animals

Eliminating the whole session file is drastic and unnecessary as we can choose to censor data from certain chambers, sensors and periods of time. For this we need to create a file named `censores.txt`, `censored.xls` or `censored.xlsx` which includes the following columns:
- Id: animal id, same as in raw files
- Date: date of experiment, same as in `raw xls` folders
- Time min: lower limit of the time interval (in minutes) where to censor data. If empty, data will be censored from the begining of the experiment
- Time max: upper limit of the time interval (in minutes) where to censor data. If empty, data will be censored to the end of the experiment
- Censored oximetry: mark '1' to censor oximetry data and '0' to keep it
- Censored activity: mark '1' to censor horizontal activity data and '0' to keep it
- Censored rearing: mark '1' to censor vertical activity data and '0' to keep it
- Censored food: mark '1' to censor food intake data and '0' to keep it
- Censored drink: mark '1' to censor water intake data and '0' to keep it
- Comment: optional, useful to point out reasons for censoring

**Note:** Each row must represent 1 animal in 1 experiment with no duplicates.

## Executing pipeline
When the prerequisites are met we are ready to open the r script. Before running the code we must verify the following parameters in the first section:
- `processnew`: Process only new experiments (T) or all experiments (F)
- `exdying`: Exclude dying animals (default is T)
- `switchtime`: Switching time in experiments (2 is common)
- `torporlim`: If energy expenditure drops under this threshold, we consider the animal is in torpor (80 is reasonable)
- File location: we may set a working directory `setwd` and indicate the `baseroute` where to look for the input and export the output.

When we run the program we obtain the following output:
- `rawdata.txt`: tabulated metabolic data for each animal, experiment and time
- `censored&rawdata.txt`: tabulated metabolic data without excluding censored information
- `meandata.txt`: mean metabolic data for eac animal and experiment
- `info.txt`: information for each experiment (experiment, module, cage, id...)
- Plots:
  - Plots per subject: time series of each main metabolic parameter for each animal and experiment
  - EE comparison per experiment: energy expenditure of each animal for each experiment
  - Activity comparison per experiment: activity of each animal for each experiment
  - Intake comparison per experiment: food and water intake of each animal for each experiment
  - Plots per parameter: boxplots representing the mean data for each animal and experiment, segmenting for light cycle and torpor state

It is advised to check the output after executing the script to detect possible artifacts or problems. If this is the case, we can modify the `censored` file and run the code again.

**Note:** If an `.xls` file fails to be imported, open it in Excel, make sure contents are complete/non-corrupted, save file as `.xls` and try to execute the script again.
