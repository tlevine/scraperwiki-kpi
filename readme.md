ScraperWiki Key Performance Indicators
======

Generate the table of interest on the server.
[This script](https://bitbucket.org/ScraperWiki/scraperwiki/src/a25f71204062/web/kpi/dump_user_table.sh) might help.

    SELECT username, last_login, date_joined FROM auth_user;

Download it.

    scp -P 7822 scraperdeploy@rush.scraperwiki.com:~/2012-04-23.csv .

Generate the plots.

    ./kpi.r

Look at them.

    evince coder_activity.pdf
