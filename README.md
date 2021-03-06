This software is intended to be used locally (not on a server) with R and Shiny.

Additional details can be found [here](https://biostat.app.vumc.org/wiki/Main/MatchedRandomization).

# Install packages

You'll need `shiny`, `shinyjs`, and `nbpMatching`.

```{r}
install.packages(c('shiny','shinyjs','nbpMatching'))
```

# Run Shiny

* Save this repository and start R from that location.

```{r}
# use whatever port you want, leave blank for random assignment
shiny::runApp(port = 3456)
```

I recommend setting a port number. The value doesn't matter so much, but using a consistent port number will allow you to use the same link each time.

* Open your web browser to [http://localhost:3456](http://localhost:3456)
