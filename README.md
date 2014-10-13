Auto-Pairing
============
This repo provides code to automatically pair mock trial tournaments according to the [AMTA tabulation manual](http://www.collegemocktrial.org/Tabulation%20Manual%20(Aug.%202014).pdf).

# Dependencies
This code is written in Rmarkdown, a variation of R that exports nicely to html.

Easiest way to edit and run is to open in RStudio and hit 'compile html'.

# How to Use it
There are two types of pairing docs, one for AMTA and one for WPB.

Both versions are automatically pointed towards a Google Drive sheet, which is how you input the team names, pairings, and point differentials.

The AMTA-version is pointed [here](https://docs.google.com/spreadsheets/d/1CU4WO8heN0LBXSURLOZQXopt0UV480Cmg7h8NuBXcRY/edit?usp=sharing) (currently set up for the WashU invitational).

The WPB-version is pointed [here](https://docs.google.com/spreadsheets/d/1REb82IzLPC3S7n93CntfAaqBETdSCDXoO6DKS9VV0ro/edit?usp=sharing).

What you'll want to do is make a copy of thos worksheets and follow the directions on the first tab. Then, under the File menu, select "Publish to Web". You'll want to publish two links: one to the Running Tab sheet and one to the Teams sheet. The former will go into the `df` link and the latter the `teams` link.

Beyond that, the only configurations you need to change will be in the first code block: 

```
round <- 3 
amta <- T 
coinTie <- 'heads' 
coinR3  <- 'heads'
```

- `round` refers to round you are pairing, e.g. after round 1, this value should be 2
- `amta` should be TRUE if you are using AMTA pairing and FALSE if you are using WPB.
- `coinTie` is the coinflip you do at the beginning of the tournament to determine which team gets the higher ranking if they are tied on everything.
- `coinR3` is the coinflip you do before round three to determine whether the higher-ranked teams in each trial are assigned to Prosecution or Defense.

Once you're done, just hit 'Compile HTML' in Rstudio, and you'll get a nice read-out of how pairing worked and how impermissibles were resolved.
