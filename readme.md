NSAPH Project Template
================
Ben Sabath, Lucas Henneman (edited 6 Dec, 2021)
04/10/2019

This directory is an premade structure that can be directly cloned from
github to serve as a starting structure for new projects done as part of
the team. This document will provide basic instructions on how to use
this template to get started on your project.

# Downloading this repository

You can copy this repository directly from github to a directory of your
choice using the following command:

``` bash
git clone https://github.com/HAQ-Lab/project_template_HAQ.git <NEW_PROJECT_NAME>
```

You will likely need to set up an rsa key on your code.harvard.edu
account to use the remote git features. You can find information on how
to set that up
[here](https://help.github.com/enterprise/2.12/user/articles/adding-a-new-ssh-key-to-your-github-account).
You will need at least one key set up for each computer you are working
on.

# Setting up Git

After initially cloning the repository, the downloaded repository will
be connected to the template repository. In order to set up your own
project and clear out the old changes (i.e. what we did to create the
repository) run the following command from the repository folder:

    rm -rf .git

Then run this command to start your new git repo:

    git init

You will then need to set up your own remote repository for the project.
First, create an empty repository with the same name as the directory
you created locally (make sure to not include the readme.md option that
github suggests) for your project. We recommend using github, as you can
make it private easily and add collaborators as needed. If you would
like, we can also make a repository under HAQ-Lab and add you as a
collaborator (or we can fork your repository to have a copy of your work
for the group).

Next, run the following lines to connect the template to the new
repository:

Connecting to a github repository:

    git remote add origin https://github.com/<username>/<NEW_PROJECT_NAME>.git
    git push -u origin master

You can then use git as you normally would.

# Starting an R project

If you are using RStudio for your project, it will be useful to set up a
project. In RStudio, go to `File > New Project`, then follow the prompts
to create a project in an existing directory. When asked, choose the
“Existing Directory” option, then browse to the `<NEW_PROJECT_NAME>`
directory and complete the directions.

# Other initial setup

The readmes (including this file) will pertain to the project template
not to your project. You should edit them (and remove extra files) so
that they describe what the intent of your research project is.

# The .gitignore file

The following is the default .gitignore file included in the directory.

``` r
cat(scan(".gitignore", what="character"), sep = "\n")
```

    ## data/*.csv
    ## data/*.rds
    ## data/*.RDS
    ## results/*.rds
    ## results/*.RDS

The .gitignore file tells git which files it should ignore. Files listed
in the .gitignore will not have their changes tracked and will not be
uploaded to the remote server. This is useful as we want to avoid
tracking large files (such as data files, which are also frequently
sensitive and shouldn’t be stored on less secure systems like github).
If there are other files you would like to avoid tracking, edit the
.gitignore file to add their name.

# Directories

-   `code`
    -   The code directory should be used to store all code used as part
        of the project. If there are a large number of code files
        additional directories within the `code` directory are
        recommended, especially if there are multiple workflows. There
        should also be a description of the order code should be run in,
        or some other description of the contents and a method of
        indicating the workflow.
    -   This [github
        repository](https://github.com/covidses/analysis_scripts) is a
        good example of how you can structure your code directory. Some
        things that are done here that are good to emphasize are that
        each file is used to handle a single step of data preparation,
        rather than putting all work in a single long file. Second, the
        prepending of each file name with a number helps people
        reviewing the project know what order to run things in (and
        ensures that files are listed in the order they are run).
        Finally, the use of an Rmarkdown file to walk through the
        creation of figures in the paper helps to ensure
        reproducibility.
-   `data`
    -   This directory should contain all raw data and processed data
        used by a given project. All csv and rds files within this
        directory are assumed to be large and by default are not tracked
        by git. If there are a large number of data files additional
        internal structure is recommended. The `.gitignore` file will
        need to be updated if more structure is used. One common
        paradign is to have a `data/raw_data` directory storing unedited
        files as receinved from the source and a `data/analysis_data`
        folder containing the assembled and cleaned data ready for use
        with models.
-   `figures`
    -   This directory should contain all of the figures generated
        through the course of reaserch. The readme file in this
        directory should list the files, have a brief discription of the
        figures, and list the file that creates the figure. This
        directory should be tracked by git.
-   `reports`
    -   This directory should contain Rmarkdown files used to summarize
        and describe reserach processes and data features. The HTML or
        PDF outputs of the markdown files should also be stored in this
        directory. This directory should be tracked by git.
-   `results`
    -   This directory should be used to store objects such as models,
        tables, or other similar products of analysis. Whether or not
        files in this directory should be tracked by github is a
        question of their size and sensitivity and likely varies by
        project. By default, .RDS files in this directory will not be
        tracked, but all other files will.
