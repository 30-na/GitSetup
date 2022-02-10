# Getting started with R and Git

Student: *Mokhtar*, *Sina*

Some helpful resources for **R**:
  * [An Introduction to R](https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf)
  * [R studio cheatsheets](https://rstudio.com/resources/cheatsheets/)
  * [DataCamp getting started with tidyverse](https://www.datacamp.com/community/tutorials/tidyverse-tutorial-r)

Some helpful resources for **Git**:

  * [Happy Git with R](https://happygitwithr.com)
  * [H.Wickham intro to Git and Github with Rstudio](http://r-pkgs.had.co.nz/git.html#git-rstudio)
  
  
## Part 1: R installation

* Install [R from CRAN](https://cran.r-project.org), it is free. If you already have R installed, please check that you have a recent version (at least 3.6.0).

* Install [Rstudio](https://www.rstudio.com/products/rstudio/#Desktop), it is free. If you already have Rstudio installed, please update to the most recent version.

* Take a screen shot of your Rstudio console window at Rstudio opening that shows the R version number. Call it **RstudioRversion**.png (could also be jpg, jpeg or pdf depeding on screenshot program you use). You will be uploading this file later


## Part 2: Git installation and configuration
* Install [git](https://git-scm.com). If you use Windows, use [git for Windows](https://gitforwindows.org). In your terminal (Mac OS) or bash (installed with git for Windows), configure Git to recognize you.

`git config --global user.name “First Last"`

`git config --global user.email "email@example.com"`

* Take a screen shot of your terminal (bash) window that shows you executing the above commands. Call it **Gitconfig**.png (could also be jpg, jpeg or pdf depeding on screenshot program you use). You will be uploading this file later.

## Part 3: Integration of Git, Github and R

* Create a [Github](https://github.com) account if not yet

* Fork this repository using **Fork** at the top right corner (this will create a copy of this repository in your GitHub account)

* Clone your repository to your local machine using
`git clone address_to_folder` (make sure you are already within the folder you want it to clone to, here address_to_folder will be something like https://github.com/irinagain/GitSetup where instead of irinagain you will have your own Github account name)

* Open Rstudio and make sure you have the version control enabled, see [Rstudio version control](https://support.rstudio.com/hc/en-us/articles/200532077?version=1.1.463&mode=desktop) for further help.

* Use Rstudio to open the project associated with the local directory. You should see the Git tab appear next to Environment and History consoles which will enable you to do staging, commits, pulls and pushes from within Rstudio. You will make all the changes locally, and only then push them to Github. If you do not see the Git tab, see [Rstudio version control](https://support.rstudio.com/hc/en-us/articles/200532077?version=1.1.463&mode=desktop) for further help. You may have to restart Rstudio to see the changes.

* Take a screen shot of your Rstudio window with the top right tab showing it's opened on this project, Git panel opened and the Files displaying the folder content.  Call it **RstudioHW1Git**.png (could also be jpg, jpeg or pdf depeding on screenshot program you use). You will be uploading this file later.

## Part 4: Git basics practicing

To practice using Git and basic commits, create a separate commit for the following changes:

* Commit 1: Change the *last name*, *first name* at the top of this document to your last name/first name.

* Commit 2: Create a new folder called Screenshots. Upload 3 screen shots you made previously into this folder (*all these changes should be in one commit*).

* Push your changes to Github.

## Part 5: Simple program

You will create a simple R function that generates Y according to linear model with given X and beta vector, and given noise variance.

* Practice writing an R function in **Function_test.R**

* Make sure to make consistent commits throughout and push your changes to GitHub in the end.

## Grading

* version control/commit practices (50%)
  - name change and screenshots commits 
  - clarity, logic and frequency of all other commits

* code style/comments (25%)
  - adherence with good R style and frequent, clear comments

* correctness (25%)
  - your function is correct


