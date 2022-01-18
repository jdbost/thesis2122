# base code from https://hansenjohnson.org/post/sync-github-repository-with-existing-r-project/

# use Terminal

# move to the project directory
cd C:/Users/avata/OneDrive/Desktop/R/thesis2122

# initiate the upstream tracking of the project on the GitHub repo
git remote add origin https://github.com/jdbost/thesis2122

# changed master -> main per https://stackoverflow.com/questions/21264738/error-src-refspec-master-does-not-match-any

# pull all files from the GitHub repo (typically just readme, license, gitignore)
git pull origin main

# set up GitHub repo to track changes on local machine
git push origin main

# from https://www.youtube.com/watch?v=kL6L2MNqPHg&ab_channel=IDGTECHtalk

library(usethis)

GITHUB_PAT = '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'

use_github(protocol = 'https', auth_token = Sys.getenv("GITHUB_PAT"))
