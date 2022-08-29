# neon4cast-example

This repository is a template example for generating a forecast that is automated through GitHub actions.

## Applying this repository to a new forecast

1) Run `install.packages("renv")` in R.
2) After the installation of `renv` finishes, run `renv::activate()` and `renv::restore()`
3) Modify `forecast_model.R` to make your forecast model .  Many of the components you need to generate the forecast, including downloading NOAA weather forecasts, downloading target data, generating forecast files, generating metadata, validating files, and submitting forecasts. Avoid running the `neon4cast::submit()` function at the end of `forecast_model.R` until you are ready to submit a forecast to the Challenge.  It is important that you do NOT change the name of the file.  GitHub Actions (below) is looking for this file name. Be sure to change your `team_name` and `team_list`
4) Run `renv::snapshot()` to update the renv.lock file with any new packages that you have added.  `renv` is a package that helps manage R packages.  It ensures that GitHub actions runs the exact versions of packages that you desire. https://rstudio.github.io/renv/index.html
6) Commit and push the changes to `forecast_model.R` and `renv.lock` to Github. 

Ready to submit a forecast?

7) Uncomment the line with the function `neon4cast::submit(forecast_file = forecast_file,metadata = metadata_file, ask = FALSE)`
8) Commit and push the changes to `forecast_model.R` to Github. 

## Running forecast in GitHub actions

1) Under the actions tab, click on ".github/workflows/do_prediction.yml" on the left side.
2) Click "Run workflow", then the green "Run workflow" button. 

## Automation

The forecast in this repository is designed to run daily at 20:00 UTC.  The execution of the forecast occurs on GitHub's servers, so your local computer does not need to be turned on.  In ".github/workflow/do_prediction.yml", the lines `-cron: "* 20 * *"` define the time that the forecast is run.  In this case it is run each day at 20:00:00 UTC (note all GitHub timings are on UTC).  You can update this to run on a different schedule based on timing codes found in https://crontab.guru

To start the automated forecast generation, directly edit the ".github/workflow/do_prediction.yml" file in GitHub. Uncomment (delete the #) lines 3 and 4: 

```
on:
  workflow_dispatch:
  #schedule:
  #- cron: "0 20 * * *"
```

A video describing how to use GitHub actions for automated forecast generation can be found here: https://youtu.be/dMrUlXi4_Bo

## Running in mybinder

You can run this repo as a "binder".  The [mybinder.org](https://mybinder.org) project will convert the repository into an interactive Rstudio sesson for you. To create a binder.  Use the link below but replace "eco4cast/neon4cast-example.git" with your repository.  It will take a while to initial build the binder.  When it builds, you will see an Rstudio window.  This is the exact R configuration that GitHub will be using to run your forecast.  The use of mybinder is primarily for testing. 

https://mybinder.org/v2/gh/eco4cast/neon4cast-example.git/HEAD?urlpath=rstudio
