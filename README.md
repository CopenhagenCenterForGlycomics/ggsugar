# ggsugar

A ggplot enabled library to draw glycans using SNFG symbols and formats.

Have you ever wanted to use glycans as a point type for ggplot?

```R
p <- ggplot(mpg, aes(x=cyl,y=hwy))
p + geom_point()
```

```R
p + geom_sugar(class='galnac')
```

```R
p + geom_sugar(class='man',position='jitter')
```


## TO-DO

* The definitions for the shapes for the glycans are currently hardcoded. We would like to be
able to use a definition SVG file, and read path definitions from there, which would then be drawn natively using `grid` graphics in R.
* We should have a basic layout algorithm working, as well as basic IUPAC condensed sequence reading, so that we can plug in whatever sequence we like.