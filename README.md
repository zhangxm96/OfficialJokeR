
# OfficialJokeR

<!-- badges: start -->
<!-- badges: end -->

> R wrapper for An API of Official Joke

[Official Joke](https://github.com/15Dkatz/official_joke_api) is  a website that collects many types of official jokes.This package pulls data in JSON format and parse them to R list objects.

## Installation
Installation from github requires the devtools package to be installed.

```R
# Install OfficialJokeR from github
devtools::install_github("zhangxm96/OfficialJokeR")
```
## Usage

```R
library(OfficialJokeR)

# Retrieve random general joke,return list
joke1 <- get_joke(type = "general",choice="random",return_type="list")

# Retrieve random programming joke,return dataframe
joke2 <- get_joke(type = "programming",choice="random",return_type="dataframe")

# Retrieve ten knock-knock jokes,return list
joke3 <- get_joke(type = "knock-knock",choice="ten",return_type="list")

# Retrieve ten general jokes,return dataframe
joke4 <- get_joke(type = "general",choice="ten",return_type="dataframe")

# Retrieve data with default value
joke5 <- get_joke()
```
