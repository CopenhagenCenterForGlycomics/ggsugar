# ggsugar

A ggplot enabled library to draw glycans using SNFG symbols and formats.

Have you ever wanted to use glycans as a point type for ggplot?

```R
p <- ggplot(mpg, aes(x=cyl,y=hwy))
p + geom_point()
```

```R
p + geom_sugar(sugar='galnac')
```

```R
p + geom_sugar(sugar='man',position='jitter')
```

```R
p <- ggplot(data.frame(x=c('T','Tn','chondroitin'),y=rep(1,3),sugar=c('gal(b1-3)galnac','glc','man')))+geom_sugar(aes(x,y,sugar=sugar),size=4,align="centre")+theme_minimal()+theme(axis.text.x=element_sugar())
```

## TO-DO

* We should have a basic layout algorithm working, as well as basic IUPAC condensed sequence reading, so that we can plug in whatever sequence we like.