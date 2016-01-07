#!/bin/sh -v
# Go to master directory
pushd /Users/bfraenkel/Documents/Data-Science/Coursera/Capstone_Swiftkey/Data
# Blog
cd Blog
sed 's/[^a-zA-Z]/ /g' < en_US.blogs.txt > en_US.blogs_tmp.txt
tr '[:upper:]' '[:lower:]' < en_US.blogs_tmp.txt > en_US.blogs_clean.txt
rm en_US.blogs_tmp.txt 
# News
cd ../News
sed 's/[^a-zA-Z]/ /g' < en_US.news.txt > en_US.news_tmp.txt
tr '[:upper:]' '[:lower:]' < en_US.news_tmp.txt > en_US.news_clean.txt
rm en_US.news_tmp.txt 
# Twitter
cd ../Twitter
sed 's/[^a-zA-Z]/ /g' < en_US.twitter.txt > en_US.twitter_tmp.txt
tr '[:upper:]' '[:lower:]' < en_US.twitter_tmp.txt > en_US.twitter_clean.txt
rm en_US.twitter_tmp.txt 
popd

