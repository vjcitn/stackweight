library(ggplot2)
dat = readLines("newrep.txt")
# ltna load time and number analysis
ltna = function(x, dropstr="built|methods.found") {
zl = which(nchar(x)==0)
bad = c(zl, grep(dropstr, x))
if (length(bad)>0) x = x[-bad]
pre = grep("elapsed", x)
nms = grep("----", x, value=TRUE)
tms = x[pre+1]
sapply(tms, function(x) scan(text=x, quiet=TRUE)[3]) -> allt
libpos = grep("time.library", x)
natt = x[libpos+1]
numatt = sapply(strsplit(natt, "/"), "[", 1)
length(numatt)
numlo = sapply(strsplit(natt, "/"), "[", 2)
numloa = sapply(strsplit(numlo, " "), "[", 1)
newdf = data.frame(pkg=nms, time=allt, nload=as.numeric(numloa), nattach=as.numeric(numatt))
newdf$pkg = gsub(" ----", "", newdf$pkg)
ggplot(newdf, aes(x=nload, y=time, text=pkg)) + geom_text(aes(label=pkg))
}
ltna(dat) + ylim(4,13)
