 n <- 1000    # define sample size
    set.seed(17) # so can reproduce the results
    treat <- sample(c('a','b','c'), n, T)
    num.diseases <- sample(0:4, n, T)
    age <- rnorm(n, 50, 10)
    cholesterol <- rnorm(n, 200, 25)
    weight <- rnorm(n, 150, 20)
    sex <- sample(c('female','male'), n, T)
    label(age) <- 'Age'      # label is in Hmisc
    label(num.diseases) <- 'Number of Comorbid Diseases'
    label(cholesterol) <- 'Total Cholesterol'
    label(weight) <- 'Weight, lbs.'
    label(sex) <- 'Sex'
    units(cholesterol) <- 'mg/dl'   # uses units.default in Hmisc

    # Specify population model for log odds that Y=1
    L <- .1*(num.diseases-2) + .045*(age-50) +
      (log(cholesterol - 10)-5.2)*(-2*(treat=='a') +
          3.5*(treat=='b')+2*(treat=='c'))
    # Simulate binary y to have Prob(y=1) = 1/[1+exp(-L)]
    y <- ifelse(runif(n) < plogis(L), 1, 0)



data(faithful)
																	
 attach(faithful)

 ## EM Algorithm

 W = waiting

 s = c(0.5, 40, 90, 16, 16)

 em = function(W,s) {
 Ep = s[1]*dnorm(W, s[2], sqrt(s[4]))/(s[1]*dnorm(W, s[2], sqrt(s[4])) +
 (1-s[1])*dnorm(W, s[3], sqrt(s[5])))
 s[1] = mean(Ep)
 s[2] = sum(Ep*W) / sum(Ep)
 s[3] = sum((1-Ep)*W) / sum(1-Ep)
 s[4] = sum(Ep*(W-s[2])^2) / sum(Ep)
 s[5] = sum((1-Ep)*(W-s[3])^2) / sum(1-Ep)
 s
}

 iter = function(W, s) {
 ccs1 = em(W,s)
 for (i in 1:5) {
 if (abs(s[i]-s1[i]) > 0.0001) {
 s=s1
 iter(W,s)
 }
 else s1
 }
 s1
 }

 iter(W,s)






p = iter(W, s)

 p1<-p[1]
 p2<-p[2]
 p3<-p[3]
 p4<-p[4]
 p5<-p[5]

 Boot<-function(B){
 r<-0
 k<-0
 bootmean1 <-rep(0, B)

 bootvar1<-rep(0, B)
 bootmean2<-rep(0, B)
 bootvar2<-rep(0, B)
 for(i in 1:B){
 p<-runif(1, 0, 1)
 if(p<p1){
 boot1<-rnorm(p1*272, p2, sqrt(p4))
 bootmean1[i]<-mean(boot1)
 bootvar1[i]<-var(boot1)
 r<-r+1
 }
else{
 boot2<-rnorm((1-p1)*272, p3, sqrt(p5))
 bootmean2[i]<-mean(boot2)
 bootvar2[i]<-var(boot2)
 k<-k+1
 }
 }
meanbootm1<-sum(bootmean1)/r
 meanbootvar1<-sum(bootvar1)/r
 meanbootm2<-sum(bootmean2)/k
 meanbootvar2<-sum(bootvar2)/k
 list(meanbootm1= meanbootm1, meanbootvar1= meanbootvar1,
 meanbootm2= meanbootm2, meanbootvar2= meanbootvar2 )
 }
Boot(1000)

####Bootstrap poisson
thedata <- data.frame(y = c(5, 6, 7, 8, 9),
x1 = c(1, 1, 2, 2, 3), x2 = c(0, 1, 0, 1, 0))
glm.fit <- glm(y ~ x1 + x2, data = thedata, family = poisson)
M <- 10000
beta.store <- matrix(NA, M, 3)
X <- cbind(1, thedata$x1, thedata$x2)
beta.mle <- coef(glm.fit)
for (i in 1:M) {
m <- exp(X %*% beta.mle)
y.new <- rpois(nrow(thedata), m)
glm.fit.i <- glm(y.new ~ thedata$x1 + thedata$x2, family = poisson)
beta.store[i,] <- coef(glm.fit.i)
}




library(RGtk2)
window=gtkWindow("Sib",show=F)
window$setTitle("SiB")
button=gtkButton("Hello World")
window$add(button)
window$setDefaultSize(200,200)
#window["visible"]=TRUE
window$show()
image <- gdkPixbuf(filename = imagefile("rgtk-logo.gif"))[[1]]
 window$set(icon = image, title = "Hello World 1.0")


n <- 5000
backbone <- rnorm(n)
 ma_data <- cbind(backbone + c(rnorm(3 * (n / 4), sd = 0.1), rt(n/4, 80)),
 backbone + c(rnorm(3 * (n / 4), , 0.1), rt(n / 4, 80)))
 ma_data <- apply(ma_data, 2, function(col) col - min(col))
win <- gtkWindow(show = FALSE)
graphics <- gtkDrawingArea()
slider <- gtkHScale(min = 0.1, max = 1.00, step = 0.1)
scale_cb <- function(range) {
 par(pty = "s")
 plot(ma_data[, 1], ma_data[, 2],
 col = rgb(0, 0, 0, alpha = range$getValue()),
 xlab = "Replicate 1", ylab = "Replicate 2",
 main = "Mock expression data", pch = 19)
 }
 gSignalConnect(slider, "value-changed", scale_cb)
vbox <- gtkVBox()
 vbox$packStart(graphics, expand = TRUE, fill = TRUE, padding = 0)
 vbox$packStart(slider, expand = FALSE, fill = FALSE, padding = 0)
 win$add(vbox)
win$setDefaultSize(400,400)
 win$showAll()


main_window <- gtkWindow(show = FALSE)
 main_window["title"] <- "RGtk2 Spreadsheet"
 main_window$setDefaultSize(600, 600)
 open_cb <- function(widget, window) {
 dialog <- gtkFileChooserDialog("Choose a CSV file", window, "open",
 "gtk-cancel", GtkResponseType["cancel"], "gtk-open",
 GtkResponseType["accept"])
 if (dialog$run() == GtkResponseType["accept"]) {
 df <- read.csv(dialog$getFilename())
 load_spreadsheet(df, basename(dialog$getFilename()))
 }
 dialog$destroy()
 }
 save_cb <- function(widget, window) {
 dialog <- gtkFileChooserDialog("Enter a name for the file", window,
 "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save",
 GtkResponseType["accept"])
 if (dialog$run() == GtkResponseType["accept"])
save_file(dialog$getFilename())
dialog$destroy()
}
 quit_cb <- function(widget, window) window$destroy()
actions <- list(
 list("FileMenu", NULL, "_File"),
list("Open", "gtk-open", "_Open File", "<control>O",
 "Select a CSV file to load as a spreadsheet", open_cb),
list("Save", "gtk-save", "_Save", "<control>S",
 "Save the current spreadsheet to a CSV file", save_cb),
 list("Quit", "gtk-quit", "_Quit", "<control>Q",
 "Quit the application", quit_cb)
 )
 action_group <- gtkActionGroup("spreadsheetActions")
 action_group$addActions(actions, main_window)



create_tree_view <- function(model) {
 tree_view <- gtkTreeView(model)
 rdf <- model$getModel()$getModel()
 sapply(tail(seq_len(ncol(rdf)), -1), function(j) {
 renderer <- gtkCellRendererText()
 column <- gtkTreeViewColumn(colnames(rdf)[j], renderer, text = j - 1)
 column$setSortColumnId(j - 1)
 column$setCellDataFunc(renderer,
 function(column, renderer, model, iter)
 {
 iter <- model$convertIterToChildIter(iter)$child.iter
 child <- model$getModel()
 iter <- child$convertIterToChildIter(iter)$child.iter
 i <- rdf$getPath(iter)$getIndices()[[1]] + 1
 renderer["text"] <- as.character(rdf[i, j])
 })
 tree_view$appendColumn(column)
 })
 tree_view$setHeadersClickable(TRUE)
 if (is.null(gtkCheckVersion(2, 10, 0))) tree_view$setGridLines("both")
 tree_view
 }




no.dimnames <- function(a) {
if(!is.array(a))a=array(a)
## Remove all dimension names from an array for compact printing.
d <- list()
l <- 0
for(i in dim(a)) {
d[[l <- l + 1]] <- rep("", i)
}
dimnames(a) <- d
a
}
x=matrix(1:9,3,3)
y=8:12
no.dimnames(x)
no.dimnames(x)



require(grDevices) # for colours
tN <- table(Ni <- stats::rpois(100, lambda=5))
r <- barplot(tN, col=rainbow(20))
#- type = "h" plotting *is* 'bar'plot
lines(r, tN, type='h', col='red', lwd=2)

barplot(tN, space = 1.5, axisnames=FALSE,
        sub = "barplot(..., space= 1.5, axisnames = FALSE)")

barplot(VADeaths, plot = FALSE)
barplot(VADeaths, plot = FALSE, beside = TRUE)

mp <- barplot(VADeaths) # default
tot <- colMeans(VADeaths)
text(mp, tot + 3, format(tot), xpd = TRUE, col = "blue")
barplot(VADeaths, beside = TRUE,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = rownames(VADeaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)

hh <- t(VADeaths)[, 5:1]
mybarcol <- "gray20"
mp <- barplot(hh, beside = TRUE,
        col = c("lightblue", "mistyrose",
                "lightcyan", "lavender"),
        legend = colnames(VADeaths), ylim= c(0,100),
        main = "Death Rates in Virginia", font.main = 4,
        sub = "Faked upper 2*sigma error bars", col.sub = mybarcol,
        cex.names = 1.5)
segments(mp, hh, mp, hh + 2*sqrt(1000*hh/100), col = mybarcol, lwd = 1.5)
stopifnot(dim(mp) == dim(hh))# corresponding matrices
mtext(side = 1, at = colMeans(mp), line = -2,
      text = paste("Mean", formatC(colMeans(hh))), col = "red")

# Bar shading example
barplot(VADeaths, angle = 15+10*1:5, density = 20, col = "black",
        legend = rownames(VADeaths))
title(main = list("Death Rates in Virginia", font = 4))

# border :
barplot(VADeaths, border = "dark blue") 


# log scales (not much sense here):
barplot(tN, col=heat.colors(12), log = "y")
barplot(tN, col=gray.colors(20), log = "xy")

# args.legend
barplot(height = cbind(x = c(465, 91) / 465 * 100,
                       y = c(840, 200) / 840 * 100,
                       z = c(37, 17) / 37 * 100),
        beside = FALSE,
        width = c(465, 840, 37),
        col = c(1, 2),
        legend.text = c("A", "B"),
        args.legend = list(x = "topleft"))

require(grDevices)
pie(rep(1, 24), col = rainbow(24), radius = 0.9)

pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
    "Apple", "Boston Cream", "Other", "Vanilla Cream")
pie(pie.sales) # default colours
pie(pie.sales,
    col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
pie(pie.sales, col = gray(seq(0.4,1.0,length=6)))
pie(pie.sales, density = 10, angle = 15 + 10 * 1:6)
pie(pie.sales, clockwise=TRUE, main="pie(*, clockwise=TRUE)")
segments(0,0, 0,1, col= "red", lwd = 2)
text(0,1, "init.angle = 90", col= "red")

n <- 200
pie(rep(1,n), labels="", col=rainbow(n), border=NA,
    main = "pie(*, labels=\"\", col=rainbow(n), border=NA,..")



is.call(call) #-> FALSE: Functions are NOT calls

## set up a function call to round with argument 10.5
cl <- call("round", 10.5)
is.call(cl)# TRUE
cl
## such a call can also be evaluated.
eval(cl)# [1] 10

A <- 10.5
call("round", A)        # round(10.5)
call("round", quote(A)) # round(A)
f <- "round"
call(f, quote(A))       # round(A)
## if we want to supply a function we need to use as.call or similar
f <- round
## Not run: call(f, quote(A))  # error: first arg must be character
(g <- as.call(list(f, quote(A))))
eval(g)
## alternatively but less transparently
g <- list(f, quote(A))
mode(g) <- "call"
g
eval(g)
## see also the examples in the help for do.call


do.call("complex", list(imag = 1:3))

## if we already have a list (e.g. a data frame)
## we need c() to add further arguments
tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
do.call("paste", c(tmp, sep=""))

do.call(paste, list(as.name("A"), as.name("B")), quote=TRUE)

## examples of where objects will be found.
A <- 2
f <- function(x) print(x^2)
env <- new.env()
assign("A", 10, envir = env)
assign("f", f, envir = env)
f <- function(x) print(x)
f(A)                                    # 2
do.call("f", list(A))                   # 2
do.call("f", list(A), envir=env)        # 4
do.call(f, list(A), envir=env)          # 2
do.call("f", list(quote(A)), envir=env) # 100
do.call(f, list(quote(A)), envir=env)   # 10
do.call("f", list(as.name("A")), envir=env) # 100

eval(call("f", A))                      # 2
eval(call("f", quote(A)))               # 2
eval(call("f", A), envir=env)           # 4
eval(call("f", quote(A)), envir=env)    # 100



require(stats) # for rnorm
plot(1:4, rnorm(4), axes = FALSE)
axis(1, 1:4, LETTERS[1:4])
axis(2)
box() #- to make it look "as usual"

plot(1:7, rnorm(7), main = "axis() examples",
     type = "s", xaxt = "n", frame = FALSE, col = "red")
axis(1, 1:7, LETTERS[1:7], col.axis = "blue")
# unusual options:
axis(4, col = "violet", col.axis="dark violet", lwd = 2)
axis(3, col = "gold", lty = 2, lwd = 0.5)

# one way to have a custom x axis
plot(1:10, xaxt = "n")
axis(1, xaxp=c(2, 9, 7))


Speed <- cars$speed
Distance <- cars$dist
plot(Speed, Distance, panel.first = grid(8,8),
     pch = 0, cex = 1.2, col = "blue")
plot(Speed, Distance,
     panel.first = lines(stats::lowess(Speed, Distance), lty = "dashed"),
     pch = 0, cex = 1.2, col = "blue")

## Show the different plot types
x <- 0:12
y <- sin(pi/5 * x)
op <- par(mfrow = c(3,3), mar = .1+ c(2,2,3,1))
for (tp in c("p","l","b",  "c","o","h",  "s","S","n")) {
   plot(y ~ x, type = tp,
        main = paste("plot(*, type = \"",tp,"\")",sep=""))
   if(tp == "S") {
      lines(x,y, type = "s", col = "red", lty = 2)
      mtext("lines(*, type = \"s\", ...)", col = "red", cex=.8)
   }
}
par(op)

##--- Log-Log Plot  with  custom axes
lx <- seq(1,5, length=41)
yl <- expression(e^{-frac(1,2) * {log[10](x)}^2})
y <- exp(-.5*lx^2)
op <- par(mfrow=c(2,1), mar=par("mar")+c(0,1,0,0))
plot(10^lx, y, log="xy", type="l", col="purple",
     main="Log-Log plot", ylab=yl, xlab="x")
plot(10^lx, y, log="xy", type="o", pch='.', col="forestgreen",
     main="Log-Log plot with custom axes", ylab=yl, xlab="x",
     axes = FALSE, frame.plot = TRUE)
my.at <- 10^(1:5)
axis(1, at = my.at, labels = formatC(my.at, format="fg"))
at.y <- 10^(-5:-1)
axis(2, at = at.y, labels = formatC(at.y, format="fg"), col.axis="red")
par(op)



vic.race<-c("white","black")
def.race<-vic.race
death.penalty<-c("yes", "no")
datalabel<-list(defendant=def.race,death=death.penalty,victim=vic.race)
table.2.6<-expand.grid(defendant=def.race,death=death.penalty,victim=vic.race)
data<-c(53, 11, 414, 37, 0, 4, 16, 139)
table.2.6<-cbind(table.2.6,count=data)
ftable(xtabs(count~defendant+death+victim ,data=table.2.6))
