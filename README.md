# EDcrowding

Welcome to the repo for the UCLH ED crowding research. 

To make best use of Github, please: 
1. Create a sub-folder for whatever you are working on eg network-analysis and initialist it with a README.md file. To create a sub-folder when on Github web, click Create new file then enter the name of the folder with a backslash after it. When you add the backslash Github will ask you to name your file - simply create an empty README.md file and your folder will be ready. Alternatively, if you have cloned this repo to your virtual desktop, create your subfolder inside the repo using File Manager or Command Prompt in Windows
2. Create a branch for your analysis, and save your work in progress inside this branch locally while working on the virtual desktop
3. Periodically commit your local work in progress branch to the remote version of your branch
4. When ready, create a pull request to merge your branch with the master branch. Github will confirm whether your branch can be safely merged without conflicts

And a couple of things I've learned the hard way
1. Don't upload zip files; Git doesn't track changes in zipped files

# A message from try-emap

This has been forked from the UCLH [try-emap](https://github.com/inform-health-informatics/try-emap) repo. Here is their information about how to get set up with some additional notes from @zmek

Welcome to UCLH's Data Science Desktop. This is a short guide to getting up and running with data science in R. We will get your tool kit ready including R, RStudio, and version control. We will then explore EMAP using dbForge and RStudio through a couple of brief vignettes.


## Setting up R

See the example `.Rprofile` and `.Renviron` files in `example-config-files`. The former just sets the CRAN mirror, and can be a good place to create paths to local installs for R packages. The latter is a good place to store local usernames and passwords, but make sure that you exclude it from version control.


## Setting up git

Git-Bash and SourceTree are both available. You may need to set-up the http.proxy. This can either be done inside the options of SourceTree, or by writing your own '~/.gitconfig' using the example file. Git needs the proxy addresses in the .gitconfig file to work. You can check this using `git config --global --list`.

## Setting up DBForge

The first time you start using this it will ask you to activate the product. Don't! It works just fine in the trial, and then the free version. You need to set-up a connection to the UDS.

![](media/devart-connection-dialog.PNG)

## Putting it all together

Let's log on to the UDS ...
