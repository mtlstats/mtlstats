# Changelog for mtlstats

## current
- Added active flag to players/goalies
- Clear rookie flag on new (regular) season
- Save a copy of the database on new season
- Implemented game standings editing

## 0.10.0
- Don't show player number zero in reports
- Fixed player/goalie name capitalisation on edit
- Return to correct edit menus after editing player stats
- Enabled batch editing of player/goalie YTD/lifetime stats
- Bugfix: allow user to edit goalie shutouts
- Added line numbers to lifetime player/goalie reports
- Implemented rookie flag

## 0.9.0
- Bugfix: Display lifetime stats in report, not YTD
- Force expected capitalization on player/goalie names
- Don't show lifetime totals in report
- Sort players in YTD and lifetime reports by points
- Moved player/goalie creation/editing to edit submenu

## 0.8.0
- Bugfix: removed quotation marks from goalie names in report
- Allow lower case player names
- Don't show players without points in game report
- Removed unnecessary goalie statistics from game report
- Fixed goalie average calculation

## 0.7.0
- Shortened views to fit within 25 lines
- Implemented goalie reports

## 0.6.0
- Generate lifetime statistics report
- Implemented goalie editing
- Reset game standings on new season

## 0.5.0
- Fixed player creation bug
- Prompt for goalie informaiton on game data entry
- Implemented player editing

## v0.4.0
- Record penalty minutes
- Calculate total game statistics
- Generate year-to-date statistics report

## v0.3.0
- Record goals and assists
- Track goals for and goals against

## v0.2.0
- Overtime losses don't count in the loss column
- Confirm game data with user before updating stats
- Implemented player creation
- Goal points are now assigned to players
- Loading/saving of database
