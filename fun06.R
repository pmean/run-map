rm(list=ls())
load("../../null/basic.RData")

# find hills

pix.dist <- function(x,y,ilog=chk$ilog,seg=c(0,0.5)) {
  tv <- get.pix(ilog)[,"tim"]
  tmax <- max(tv)
  isub <- which((tv>seg[1]*tmax) & (tv<seg[2]*tmax))
  xv <- get.pix(ilog)[isub,"x"]
  yv <- get.pix(ilog)[isub,"y"]
  d <- (xv-x)^2+(yv-y)^2
  print(min(d))
  return(which(d==min(d)))
}
h <- locator(n=1)
pix.dist(h$x,h$y)

nbh <- c(3.886081e+01,-9.463068e+01)

gpx.dist <- function(x,y,ilog=chk$ilog,seg=c(0,0.5)) {
  tv <- get.gpx(ilog)[,"tim"]
  tmax <- max(tv)
  isub <- which((tv>seg[1]*tmax) & (tv<seg[2]*tmax))
  xv <- get.gpx(ilog)[,"plat"]
  yv <- get.gpx(ilog)[,"plon"]
  adj <- cos(xv[1]*pi/180)
  d <- (xv-x)^2+(yv-y)^2
  d[-isub] <- 999
  print(min(d))
  return(which(d==min(d)))
}
i <- gpx.dist(nbh[1],nbh[2],seg=c(0.5,1))
get.gpx()[i,]


add.hill <- function(geo.loc,ilog=chk$ilog) {
  # uses get.gpx
  # used by imp.all, pac.tin
  if (verbose) cat("v1.1.1 imp.pix\n")
  library("RgoogleMaps")
  gpx.dat <- get.gpx(ilog)
  pos.mp <- LatLon2XY(geo.loc$x,geo.loc$y,get.sum(ilog,"zoom"))
  cen.mp <- LatLon2XY(get.sum(ilog,"lat"),get.sum(ilog,"lon"),get.sum(ilog,"zoom"))
  pix.hill <- sapply(Tile2R(pos.mp,cen.mp),round)
  names(pix.hill) <- c("x","y")
  text(pix.hill[1],pix.hill[2],"X",col="red",cex=2)
  return(pix.hill)
}
ilog <- 584
map.all(ilog); add.hill(nbh,ilog); ilog <- ilog-1


map.ele <- function(ilog=chk$ilog) {
  d <- NULL
  z <- NULL
  k <- NULL
  for (i in ilog) {
    d <- c(d,get.gpx(i)[,"cdst"])
    z <- c(z,get.gpx(i)[,"pele"])
    k <- c(k,rep(i,length(d)))
  }
  for (i in ilog) {
    plot(d,z,type="n")
    lines(d[k==i],z[k==i])
    title(i)
  }
  return(list(x=d,y=z,k=k))
}
sp <- map.ele()

cal.ele <- function(ilog=chk$ilog) {
  d <- get.pix(ilog)[,"cdst"]
  z <- get.pix(ilog)[,"z"]
  n <- length(z)
  dz <- c(0,z[-1]-z[-n])
  rise <- rle(dz>= 0)
  drop <- rle(dz<=-1)
  rise.y <- rep(rise$values,rise$lengths)
  rise.n <- rep(rise$lengths,rise$lengths)
  rise.n[!rise.y] <- 0
  # rise.n[rise.n<5] <- 0
  top <- which(c(diff(rise.n)<0,FALSE))
  bot <- which(c(FALSE,diff(rise.n)>0))
  if(length(top)>length(bot)) {bot <- c(bot,n)}
  k <- length(top)
  large.gap <- abs(top[c(2:k,1)]-bot)>4
  top <- top[c(TRUE,large.gap[-k])]
  bot <- bot[c(large.gap[-k],TRUE)]
  rise.ht <- z[top]-z[bot]
  top <- top[rise.ht>10]
  bot <- bot[rise.ht>10]
  rise.ht <- z[top]-z[bot]
  return(data.frame(top=top,bot=bot,rise.ht=rise.ht))
}
cal.ele()

ilog <- 582
win.graph(width=640,height=320)
par(mfrow=c(1,2),xaxs="i",yaxs="i")
hills <- cal.ele(ilog)
par(mar=rep(0,4),xaxs="i",yaxs="i")
plot(c(-160,160),c(-160,160),type="n",axes=FALSE)
map.ras <- get.ras(ilog)
rasterImage(map.ras,-160,-160,160,160)
map.rte(ilog)
map.mrk(ilog)
map.ele(ilog)

# cal.bar function 

int.dst <- function(ilog=chk$ilog) {
  gpx.dat <- get.gpx(ilog)
  tmax <- trunc(max(gpx.dat[,"tim"])/60)
  tim.per.min <- rep(-1,tmax)
  dst.per.min <- rep(-1,tmax)
  for (i in 1:tmax) {
    tim.per.min[i] <- i
    dst.per.min[i] <-  round(cal.ing(gpx.dat,"tim",60*i,"cdst"))
  }
  return(list(tim=1:tmax,dst=dst.per.min))
}
int.dst()

int.kph <- function(ilog=chk$ilog) {
  dst <- int.dst(ilog)
  speed <- round(diff(c(0,dst$dst))*60/1000,1)
  list(x=dst$tim,y=speed)
}
int.kph()

int.tim <- function(ilog=chk$ilog) {
  gpx.dat <- get.gpx(ilog)
  dmax <- max(gpx.dat[,"cdst"])
  dinc <- 200 + 300*(dmax>1500) + 500*(dmax>5500)
  dnum <- trunc(dmax/dinc)
  tim.per.km <- rep(-1,dnum)
  dst.per.km <- (1:dnum)*dinc
  for (i in 1:dnum) {
    tim.per.km[i] <-  round(cal.ing(gpx.dat,"cdst",i*dinc,"tim"))
  }
  return(list(tim=tim.per.km,dst=dinc*(1:dnum)))  
}
int.tim()


cal.lin <- function(sp) {
  m0 <- lm(sp$y~sp$x)
  p0 <- predict(m0)
  n <- length(p0)
  segments(1,p0[1],n,p0[n])
  r <- quantile(resid(m0),probs=c(0.25,0.75))
  segments(0.5,p0[1]+r,n-0.5,p0[n]+r)
  drw.txt(1,p0[1],round(p0[1],1))
  drw.txt(n,p0[n],round(p0[n],1))
  avg <- 0.50*(p0[1]+p0[n])
  drw.txt((n+1)/2,avg,round(avg,1))
}

cal.bar <- function(ilog=chk$ilog) {
  sp <- int.kph(ilog)
  tim <- int.tim(ilog)
  
  par(mar=c(2.6,2.6,0.6,0.6))
  plot(sp,type="l",xlim=c(0,max(sp$x)),ylim=c(0,12),axes=FALSE)
  abline(v=tim$tim/60,col="gray")
  lb <- paste(fmt.tim(tim$tim),paste(tim$dst/1000,"K",sep=""),sep="\n")
  axis(side=1,at=tim$tim/60,labels=lb)
  axis(side=2)
  box()
  cal.lin(sp)
  return(sp)
}
cal.bar()

drw.txt <- function(x,y,tx) {
  w <- strwidth(tx)
  h <- strheight(tx)
  bx <- list(x=x+w*c(-1,-1, 1, 1),y=y+h*c(-1, 1, 1,-1))
  polygon(bx,col="gray",border="white")  
  text(x,y,tx)
}

# basic functions

celsius <- function (f) {5 * (f - 32)/9}

clr <- function() {polygon(x=c(160,160,320,320),y=c(-160,160,160,-160),density=-1,col="white")}

km <- function (mi) {1.61 * mi}

mi <- function (km) {0.62 * km}

pace <- function (km, ti.min, ti.sec = 0) {(ti.min + ti.sec/60) * 5/km}


# cal functions
#   calculate internal values

cal.cel <- function(f) {5 * (f - 32)/9}

fmt.day <- function(day) {
  nday <- as.numeric(day)
  paste(nday%/%10000,zpad((nday%%10000)%/%100),zpad(nday%%100),sep="-")
}
fmt.day(19560622) # test fmt.day
fmt.day("19600606") # test fmt.day

fmt.dan <- function(day) {
  as.numeric(as.Date(day,"%Y%m%d")-as.Date("20110101","%Y%m%d"))
}
fmt.dan("20111231")

cal.ing <- function(g,xnam,xtarget,ynam) {
  # interpolation within a matrix
  # uses function cal.inv
  # used by function cal.mrk
  cal.inv(g[,xnam],xtarget,g[,ynam])
}

cal.inv <- function(x,xtarget,y) {
  # interpolation within a vector
  # used by function cal.ing
  if (sum(x==xtarget)>0) {return(mean(y[x==xtarget]))}
  if (sum(x<xtarget)==0) {return(mean(y[x==min(x)]))}
  if (sum(x>xtarget)==0) {return(mean(y[x==max(x)]))}
  n <- length(x)
  o <- order(x)
  xs <- x[o]
  ys <- y[o]
  i <- sum(xs<=xtarget)
  j <- i+1
  i <- max(i,1)
  j <- min(j,n)
  dx <- xs[j]-xs[i]
  dt <- min(xtarget-xs[i],dx)
  w <- ifelse(dx==0,1,1-dt/dx)
  w*ys[i]+(1-w)*ys[j]
}
cal.inv(c(10,20,30,40,50),20,1:5) # test cal.inv
cal.inv(c(10,20,20,40,50),20,1:5) # test cal.inv
cal.inv(c(10,20,20,20,50),20,1:5) # test cal.inv
cal.inv(c(10,20,30,40,50),50,1:5) # test cal.inv
cal.inv(c(10,20,30,50,50),50,1:5) # test cal.inv
cal.inv(c(10,20,50,50,50),50,1:5) # test cal.inv
cal.inv(c(10,20,30,40,50),21,1:5) # test cal.inv
cal.inv(c(10,20,30,40,50),25,1:5) # test cal.inv
cal.inv(c(10,20,30,40,50),28,1:5) # test cal.inv
cal.inv(c(10,20,30,40,50), 9,1:5) # test cal.inv
cal.inv(c(10,20,30,40,50),51,1:5) # test cal.inv

cal.kil <- function(cdst) {
  ndst <- as.numeric(cdst)
  paste(round(ndst/1000,1),"K",sep="")
}
cal.kil(1563)

cal.mil <- function (km) {0.62*km}

cal.pac <- function (km, ti.min, ti.sec = 0) {(ti.min + ti.sec/60) * 5/km}

fmt.tim <- function(tim) {
  ntim <- as.numeric(tim)
  n <- length(tim)
  ftm <- rep("99:99:99",n)
  for (i in 1:n) {
    tv <- c(ntim[i]%/%3600,(ntim[i]%/%60)%%60,ntim[i]%%60)
    ftm[i] <- ifelse(tv[1]>0,
                     paste(tv[1],zpad(tv[2]),zpad(tv[3]),sep=":"),
                     paste(tv[2],zpad(tv[3]),sep=":"))
  }
  return(ftm)
}
fmt.tim(c(0,1,59,60,61,599,600,601,3599,3600,3601,35999,36000,36001))
fmt.tim(c("599","601"))

# set up chk values for quality testing

chk <- list(ilog=303,r="nobrake1",idx=103,j=3)
verbose=TRUE

# ext functions
#   extract information

ext.num <- function(g,gbeg=1,gend=nchar(g)) {
  # used by imp.day, imp.geo, imp.tim
  if (verbose) cat("v1.1.1 ext.num\n")
  strn <- substr(g,gbeg,gend)
  numb <- as.numeric(strn)
  chk <- is.na(numb)
  if (sum(chk)>0) {print(g[chk]); stop("***Conversion problems***")}
  return(numb)
}
ext.num("38.8560653")

ext.str <- function(g,str0,str1) {
  # used by imp.tim, imp.day, imp.geo
  if (verbose) cat("v1.1.1 ext.str\n")
  gbeg <- regexpr(str0,g)
  gend <- regexpr(str1,g)
  strn <- substr(g,gbeg+nchar(str0),gend-1)
  tst0 <- sum(gbeg==-1)
  tst1 <- sum(gend==-1)
  strn
}  
ext.str("<trkpt lat=38.8560653 lon=-94.6306661> r02/Track 141.gpx","lat="," lon=")

ext.sub <- function(tag,idx=chk$idx,j=chk$j) {
  # used by imp.tim, imp.day, imp.geo
  if (verbose) cat("v1.1.1 ext.sub\n")
  f.nam <- paste("r0",j,"/Track ",zzpad(idx),".gpx",sep="")
  gpx <- read.table(file=f.nam,header=FALSE,as.is=TRUE,sep="~")
  gvec <- gpx$V1
  paste(as.vector(gvec[grepl(tag,gvec)]),f.nam)
}
tail(ext.sub("<ele>")); tail(ext.sub("<trkpt"))

# imp functions 
#   import information from files/Internet

# level 0: stand-alone

imp.day <- function(idx=chk$idx,j=chk$j) {
  # uses ext.sub, ext.str, ext.num
  # used by imp.gpx
  if (verbose) cat("v1.1.1 imp.day\n")
  gtim <- ext.sub("<time>",idx,j)
  gymd <- ext.str(gtim,"<time>","T")
  tyr <- ext.num(gymd,1,4)
  tmo <- ext.num(gymd,6,7)
  tda <- ext.num(gymd,9,10)
  return(tyr*10000+tmo*100+tda)
}
imp.day()

imp.tim <- function(idx=chk$idx,j=chk$j) {
  # uses ext.sub, ext.str, ext.num
  # used by imp.gpx
  if (verbose) cat("v1.1.1 imp.tim\n")
  gtim <- ext.sub("<time>",idx,j)
  ghms <- ext.str(gtim,"T","Z")
  tda <- ext.num(gtim,15,16)
  thr <- ext.num(ghms,1,2)
  tmi <- ext.num(ghms,4,5)
  tse <- ext.num(ghms,7,8)
  tim <- thr*60*60+tmi*60+tse
  k <- which(tim==min(tim))[1]
  if (k!=1) {tim[1:(k-1)] <- tim[1:(k-1)]-24*60*60}
  return(tim-min(tim))
}
imp.tim()

imp.geo <- function(idx=chk$idx,j=chk$j) {
  # uses function ext.num, ext.sub
  # used by imp.gpx
  if (verbose) cat("v1.1.1 imp.geo\n")
  ilog <- 800*(j==1)+200*(j==3)+idx
  f.nam <- paste("r0",j,"/Track ",zzpad(idx),".gpx",sep="")
  gpos <- ext.sub("<trkpt",idx,j)
  gele <- ext.sub("<ele>",idx,j)
  plat <- ext.num(ext.str(gpos,"lat=" ," lon=" ))
  plon <- ext.num(ext.str(gpos,"lon=" ,">"     ))
  pele <- ext.num(ext.str(gele,"<ele>","</ele>"))
  geo.dat <- cbind(rep(ilog,length(plat)),plat,plon,pele)
  dimnames(geo.dat)[[2]] <- c("i","plat","plon","pele")
  return(geo.dat)
}
tail(imp.geo()) # test

# level 1: uses level 0 imp functions

imp.dst <- function(idx=chk$idx,j=chk$j) {
  # uses imp.geo
  # used by imp.gpx
  if (verbose) cat("v1.1.1 imp.dst\n")
  geo.dat <- imp.geo(idx,j)
  n <- dim(geo.dat)[1]
  k <- c(1,1:(n-1))
  rad.meters <- 6371*1000 
  per.meters <- 2*pi*rad.meters
  lat.meters <- per.meters/360
  adj <- cos(geo.dat[1,"plat"]*pi/180)
  lon.meters <- adj*lat.meters
  lat.dist <- lat.meters*(geo.dat[,"plat"]-geo.dat[k,"plat"])
  lon.dist <- lon.meters*(geo.dat[,"plon"]-geo.dat[k,"plon"])
  inc.dist <- sqrt(lat.dist^2+lon.dist^2)
  ang.radi <- atan2(lat.dist,lon.dist)
  ang.degr <- round(ang.radi*180/pi + 360*(ang.radi<0))
  dst.dat <- as.matrix(round(cumsum(inc.dist)))
  dimnames(dst.dat)[[2]] <- list("cdst")
  ang.dat <- as.matrix(ang.degr)
  dimnames(ang.dat)[[2]] <- list("pang")
  return(cbind(geo.dat,dst.dat,ang.dat))
}  
tail(imp.dst())

# level 2: uses level 1 imp functions

imp.gpx <- function(ilog=chk$ilog,save=FALSE) {
  # calls imp.geo, imp.tim, imp.day
  # called by sav.gpx
  if (verbose) cat("v1.1.1 imp.gpx\n")
  idx <- ilog - 200*(ilog>200)-600*(ilog>800)
  j <- 2+as.numeric((ilog>200)&(ilog<800))-as.numeric(ilog>800)
  f.nam <- paste("r0",j,"/Track ",zzpad(idx),".gpx",sep="")
  ilab <- paste("log",ilog,sep="")
  dst.dat <- imp.dst(idx,j)
  day.dat <- imp.day(idx,j)
  hms.dat <- imp.tim(idx,j)
  tim.dat <- cbind(day.dat,hms.dat)
  dimnames(tim.dat)[[2]] <- c("day","tim")
  gpx.dat <- cbind(dst.dat,tim.dat)
  n <- dim(gpx.dat)[1]
  dimnames(gpx.dat)[[1]] <- paste(ilab,c("beg",zzpad(2:(n-1)),"end"),sep=".")
  if (save) {dput(gpx.dat,file=paste("data/gsub",zzpad(ilog),".dat",sep=""))}
  return(gpx.dat)
}
tail(imp.gpx()) # test

get.gpx <- function(ilog=chk$ilog) {
  # gets previously stored information about run i
  # used by imp.cen, imp.pix, pac.beg
  if (verbose) cat("v1.1.1 get.gpx\n")
  gpx.dat <- dget(paste("data/gsub",zzpad(ilog[1]),".dat",sep=""))
  for (j in ilog[-1]) {
    gpx.sub <- dget(paste("data/gsub",zzpad(j),".dat",sep=""))
    gpx.dat <- rbind(gpx.dat,gpx.sub)
  }
  return(gpx.dat)
}
tail(get.gpx()) # test

# level 3: uses level 2 imp functions

imp.cen <- function(ilog=chk$ilog,sz=320,gmar=10) {
  # calculates the range, midrange
  # and zoom for latitude and longitude
  # uses get.gpx, MaxZoom 
  # used by imp.ras, imp.sum
  if (verbose) cat("v1.1.1 imp.cen\n")
  library("RgoogleMaps")
  gpx.dat <- get.gpx(ilog)
  rlat <- range(gpx.dat[,"plat"])
  rlon <- range(gpx.dat[,"plon"])
  mlat <- mean(rlat)
  mlon <- mean(rlon)
  zm <- MaxZoom(rlat,rlon,siz=rep(sz-2*gmar,2))
  map.cen <- c(mlat,mlon,zm)
  names(map.cen) <- c("lat", "lon","zoom")
  return(map.cen)
}
imp.cen(); imp.cen(c(303,311))

imp.fin <- function(ilog=chk$ilog) {
  # uses get.gpx 
  # used by imp.sum
  if (verbose) cat("v1.1.1 imp.fin\n")
  library("RgoogleMaps")
  gpx.dat <- get.gpx(ilog)
  n <- dim(gpx.dat)[1]
  vlist <- c("cdst","tim","day")
  fin <- as.vector(gpx.dat[n,vlist])
  names(fin) <- vlist
  return(fin)
}
imp.fin()

# level 4: uses level 3 imp functions

imp.ras <- function(ilog=chk$ilog, sz=320, save=FALSE) {
  # uses imp.cen
  # used by imp.all
  if (verbose) cat("v1.1.1 imp.ras\n")
  cen.dat <- imp.cen(ilog)
  library("RgoogleMaps")
  map.goo <- GetMap(destfile="mapj.png",center=cen.dat[c("lat","lon")],size=rep(sz,2),
    zoom=cen.dat["zoom"],maptype="hybrid")
  map.ras <- map.goo$myTile
  if (save) {save(map.ras,file=paste("data/msub",zzpad(ilog),".RData",sep=""))}
  cen.var <- c("lat.center","lon.center","zoom","BBOX","url","size","SCALE")
  cen.goo <- map.goo[cen.var]
  return(cen.goo)
}
imp.ras()

get.ras <- function(ilog=chk$ilog) {
  # used by map.beg
  if (verbose) cat("v1.1.1 get.ras\n")
  if (length(ilog) > 1) {print("Warning: requesting multiple maps in get.ras"); ilog <- ilog[1]}
  load(paste("data/msub",zzpad(ilog),".Rdata",sep=""))
  if (any(dim(map.ras)!=320)) {print("Raster image not found in get.ras"); stop()}
  map.ras
}
dim(get.ras())

imp.sum <- function(ilog=chk$ilog, rname=chk$r, rtype="Mission",nt="no notes",sort=FALSE, save=FALSE) {
  # add information about new run to matrix of old runs
  # uses functions imp.fin, imp.cen
  # used by function imp.all
  if (verbose) cat("v1.1.1 get.ras\n")
  s <- read.csv(file="infrun.csv",row.names=1,as.is=TRUE)
  k <- dim(s)[1]
  s.new <- s[c(1:k,k),]
  fin.dat <- imp.fin(ilog)
  cen.dat <- imp.cen(ilog)
  ext <- round((5000/fin.dat["cdst"])^1.06*fin.dat["tim"]/60,1)
  typ.dat <- c(ilog,rname,rtype,ext,nt)
  names(typ.dat) <- c("ilog","rname","rtype","ext","nt")
  cmb.dat <- c(fin.dat,cen.dat,typ.dat)
  s.new[k+1,names(cmb.dat)] <- cmb.dat
  n.days <- sum(s.new$day==s.new$day[k+1])
  row.names(s.new)[k+1] <- paste(letters[n.days], s.new$day[k+1], sep="")
  o <- order(s.new[,"day"],row.names(s.new))
  if (sort) {s.new <- s.new[o,]}
  if (save==FALSE) {return(s.new)}
  write.csv(s.new,file="infrun.csv")
  return(s.new)
}
tail(imp.sum()) # test

get.sum <- function(ilog=chk$ilog, vlist=TRUE) {
  if (verbose) cat("v1.1.1 get.sum\n")
  run.dat <- read.csv(file="infrun.csv",row.names=1,as.is=TRUE)
  selection <- run.dat$ilog %in% ilog
  if (sum(selection)==0) {print("no rows selected")}
  sum <- run.dat[selection, vlist]
  if ((vlist!=TRUE)&(vlist!="rname")) {sum <- as.numeric(sum)}
  return(sum)
}
get.sum(); get.sum(chk$ilog,"cdst"); get.sum(chk$ilog,"rname")

get.rnm <- function(r=chk$r, vlist=TRUE) {
  if (verbose) cat("v1.1.1 get.rnm\n")
  run.dat <- read.csv(file="infrun.csv",row.names=1,as.is=TRUE)
  sum <- run.dat[run.dat$rname %in% r, vlist]
  if (vlist!=TRUE) {sum <- as.numeric(sum)}
  return(sum)
}
get.rnm(); get.rnm(c("nobrake1","nobrake2"),"cdst");

get.sub <- function(ilog=chk$ilog,r=chk$r) {
  run.dat <- get.rnm(r=r)
  iday <- run.dat$day[run.dat$ilog==ilog]
  run.dat[run.dat$day <= iday,]
}
get.sub(); get.sub(ilog=408)

# level 5: uses level 4 imp functions

imp.all <- function(ilog, rnm, rty, nt) {
  # uses function imp.gpx, imp.cen, imp.ras, imp.sum
  all.races <- get.sum(1:999); print(tail(all.races))
  ilog.check <- rev(all.races$ilog)[1]
  if (ilog.check==ilog) {stop("Operator error: duplicate ilog value")}
  if (verbose) cat("v1.1.1 imp.all\n")
  imp.gpx(ilog,save=TRUE)
  imp.ras(ilog,save=TRUE)
  run.dat <- imp.sum(ilog, rname=rnm, rtype=rty, nt=nt, sort=TRUE, save=TRUE)
  imp.pix(ilog,save=TRUE)
  return(tail(run.dat))
}
# don't test this function willy-nilly

# pix functions
#  calculate intermediate values

imp.pix <- function(ilog=chk$ilog,imap=ilog[1],save=FALSE) {
  # uses get.gpx
  # used by imp.all, pac.tin
  if (verbose) cat("v1.1.1 imp.pix\n")
  library("RgoogleMaps")
  gpx.dat <- get.gpx(ilog)
  pos.mp <- LatLon2XY(gpx.dat[,"plat"],gpx.dat[,"plon"],get.sum(imap,"zoom"))
  cen.mp <- LatLon2XY(get.sum(imap,"lat"),get.sum(imap,"lon"),get.sum(imap,"zoom"))
  pos.dat <- sapply(Tile2R(pos.mp,cen.mp),round)
  names(pos.dat) <- tolower(names(pos.dat))
  zele <- round(gpx.dat[,"pele"]-min(gpx.dat[,"pele"]))
  pix.dat <- data.frame(gpx.dat[,c("i","cdst","pang","tim")],pos.dat,z=as.matrix(zele))
  names(pix.dat) <- tolower(names(pix.dat))
  if (save) {dput(pix.dat,paste("data/psub",zzpad(ilog),".dat",sep=""))}
  return(pix.dat)
}
tail(imp.pix()); head(imp.pix(c(303,311))); tail(imp.pix(c(303,311)))

get.pix <- function(ilog=chk$ilog) {
  # used by map.fin, cal.mrk, map.rte
  if (verbose) cat("v1.1.1 get.pix\n")
  return(dget(paste("data/psub",zzpad(ilog),".dat",sep="")))
}
tail(get.pix()) # test

# map functions 

# level 0: stand-alone

map.beg <- function(ilog=chk$ilog, pix=320, xmar=160) {
  # uses get.ras
  # used by map.all
  if (verbose) cat("v1.1.1 map.beg\n")
  map.ras <- get.ras(ilog)
  par(mar=rep(0,4),xaxs="i",yaxs="i")
  plot(c(-pix/2, xmar+pix/2),c(-pix/2, pix/2),type="n",axes=FALSE)
  rasterImage(map.ras, -pix/2, -pix/2, pix/2, pix/2)
  return(dim(map.ras))
}
map.beg() # test

# level 1: uses level 0 map functions (implicitly)

map.cir <- function(x,y,an,d,lb,off.d=d,co="blue",pts=32) {
  # used by map.mrk
  if (verbose) cat("v1.1.1 map.cir\n")
  ra <- 2*pi*(an-90)/360
  off.x <- x+off.d*cos(ra)
  off.y <- y+off.d*sin(ra)
  ci <- seq(0,2*pi,length=pts)
  xc <- off.x+d*cos(ci)
  yc <- off.y+d*sin(ci)
  polygon(xc,yc,density=-1,col=co,border=co)
  cx <- ifelse(nchar(lb)==1,1,0.75)
  text(off.x,off.y,lb,cex=cx,col="white")
}
map.beg(); map.cir(0,0,0,20,1); map.cir(0,0,180,20,1.5); map.cir(0,0,0,10,"CC",0)

map.dia <- function(x,y,d,co) {
  # used by map.fin
  if (verbose) cat("v1.1.1 map.dia\n")
  x0 <- x-d*sqrt(2)
  x1 <- x+d*sqrt(2)
  y0 <- y-d*sqrt(2)
  y1 <- y+d*sqrt(2)
  xc <- c(x0,x,x1,x)
  yc <- c(y,y0,y,y1)
  polygon(xc,yc,density=-1,col=co)
  cbind(xc,yc)
}
map.beg(); map.dia(0,0,20,"red")

map.sqr <- function(x,y,d,co) {
  # used by map.fin
  if (verbose) cat("v1.1.1 map.sqr\n")
  x0 <- x-d
  x1 <- x+d
  y0 <- y-d
  y1 <- y+d
  xc <- c(x0,x0,x1,x1)
  yc <- c(y0,y1,y1,y0)
  polygon(xc,yc,density=-1,col=co)
  cbind(xc,yc)
}
map.beg(); map.sqr(0,0,10,"blue")

# level 2: uses level 1 map funtions

map.dvc <- function(ds) {
  # used by cal.mrk
  if (verbose) cat("v1.1.1 map.dvc\n")
  n <- length(ds)
  dend <- ds[n]
  dinc <- 200 + 300 * (dend >  1500) +  500 * (dend > 5400) 
  if(dend < 1000) dinc <- dend/2
  dbeg <- min(dinc,dend)
  return(unique(c(0,seq(dbeg,dend,by=dinc),dend)))
}
map.dvc((1:502)*10)

cal.mrk <- function(ilog=chk$ilog) {
  # uses get.pix, map.dvc
  if (verbose) cat("v1.1.1 cal.mrk\n")
  pix.dat <- get.pix(ilog)
  dmrk <- map.dvc(pix.dat[,"cdst"])
  mrk.dat <- matrix(0,nrow=length(dmrk),ncol=6)
  dimnames(mrk.dat)[[2]] <- c("x","y","cdst","tm","pang","speed")
  for (i in 1:length(dmrk)) {
    mrk.dat[i,   "x"] <- round(cal.ing(pix.dat,"cdst",dmrk[i],"x"))
    mrk.dat[i,   "y"] <- round(cal.ing(pix.dat,"cdst",dmrk[i],"y"))
    mrk.dat[i,  "tm"] <- round(cal.ing(pix.dat,"cdst",dmrk[i],"tim"))
    mrk.dat[i,"pang"] <- round(cal.ing(pix.dat,"cdst",dmrk[i],"pang"))
  }
  mrk.dat[,"cdst"] <- round(dmrk/1000,2)
  mrk.dat[-1,"speed"] <- round(3600*diff(c(mrk.dat[,"cdst"]))/diff(c(mrk.dat[,"tm"])),1)
  return(mrk.dat)
}
tail(cal.mrk()) # test

map.mrk <- function(ilog=chk$ilog) {
  # uses cal.mrk
  # used by map.all
  if (verbose) cat("v1.1.1 map.mrk\n")
  delta <- 20
  cm <- cal.mrk(ilog)
  n <- dim(cm)[1]
  mrk.lab1 <- paste(cm[,"cdst"],"K,",sep="")
  mrk.lab2 <- paste(fmt.tim(cm[,"tm"]),",",sep="")
  mrk.lab3 <- paste(cm[,"speed"],"KPH",sep="")
  xpos <- 170
  ypos <- 155
  text(xpos,ypos,fmt.day(get.sum(ilog,"day")),adj=0)
  text(xpos,ypos-delta,"Intermediate times",adj=0)
  map.sqr(cm[1,"x"],cm[1,"y"],8,"blue")
  text(cm[1,"x"],cm[1,"y"],"0",col="white")
  for (i in 2:(n-1)) {
    map.cir(cm[i,"x"],cm[i,"y"],cm[i,"pang"],8,cm[i,"cdst"])
  }
  map.dia(cm[n,"x"],cm[n,"y"],8,"blue")
  text(cm[n,"x"],cm[n,"y"],cm[n,"cdst"],col="white")
  for (i in 2:n) {
    text  (xpos,    ypos-i*delta,mrk.lab1[i],adj=0)
    text  (xpos+ 80,ypos-i*delta,mrk.lab2[i],adj=1)
    text  (xpos+140,ypos-i*delta,mrk.lab3[i],adj=1)
  }
  mrk.labs <- cbind(mrk.lab1,mrk.lab2,mrk.lab3)
  return(mrk.labs)
}
map.beg(); map.mrk()

map.rte <- function(ilog=chk$ilog,opt.lwd=5,opt.col="blue",opt.lty="solid") {
  if (verbose) cat("v1.1.1 map.rte\n")
  p <- get.pix(ilog)
  n <- dim(p)[1]
  segments(p[-n,"x"],p[-n,"y"],p[-1,"x"],p[-1,"y"],lwd=opt.lwd,lty=opt.lty,col=opt.col)
  c(range(p[,"x"]),range(p[,"y"]))
}
map.beg(); map.rte()

# Level 3

map.all <- function(ilog=chk$ilog) {
  if (verbose) cat("v1.1.1 get.pix\n")
  if (length(ilog)>1) {print("***Duplicate runs***"); return(-1)}
  map.beg(ilog)
  map.rte(ilog)        
  map.mrk(ilog)       
  return(ilog)
}
map.all()

# Level 4

map.png <- function(ilog=chk$ilog) {
  library("RgoogleMaps")
  pth <- paste(def.path,"/pmean/personal/log",sep="")
  fnm <- paste(pth,"/log",zzpad(ilog),".png",sep="")
  png(file=fnm,width=480,height=320)
  map.all(ilog)
  dev.off()
}

# pac functions
#   compare runs across same route

pac.lst <- function(r=chk$r,vname="ilog") {
  i.list <- get.rnm(r,vname)
  t.list <- get.rnm(r,"tim")
  o <- order(t.list)
  return(i.list[o])
}
pac.lst(); pac.lst(vname="day")

pac.bst <- function(r=chk$r,ilog=chk$ilog) {
  rs <- get.sub(r=r,ilog=ilog)
  t.list <- round(rs$tim/30)/2
  n <- dim(rs)[1]
  if (n==1) return ("first time")
  tj <- t.list[n]
  t0 <- min(t.list[-n])
  if (tj<t0) return(paste(t0-tj,"minutes faster than my previous best time"))
  if (tj==t0) return("as fast as my previous best time")
return(paste(tj-t0,"minutes slower than my previous best time"))
}
pac.bst(); pac.bst(i=812)

# hdr functions
#   create html code

# Level 0

# Lines 1-2
hdr.dat <- function(ilog=chk$ilog) {
  # <p><strong>Saturday, November 17<a name="r1117"></a>,
  if (verbose) cat("v1.1.1 hdr.dat\n")
  library("chron")
  sp <- " ";  comma <- ", "; crt <- "\n"; 
  day.name <- c("Sunday", "Monday", "Tuesday",
                "Wednesday", "Thursday", "Friday", "Saturday")
  dat  <- get.sum(ilog,"day")
  dd   <- as.numeric(substr(dat,7,8))
  mm   <- as.numeric(substr(dat,5,6))
  yyyy <- as.numeric(substr(dat,1,4))
  mon  <- month.name[mm]
  day  <- day.name[1+day.of.week(mm,dd,yyyy)]
  c('<p><strong>',day,comma,mon,sp,dd,comma,yyyy,crt,
    '<a name="r',zpad(mm),zpad(dd),'"></a>',comma,crt)
}
cat(hdr.dat(),sep="")

# Line 3
hdr.dst <- function(ilog=chk$ilog) {
  # 4.7K in 35 minutes</strong></p>
  if (verbose) cat("v1.1.1 hdr.dst\n")
  dst <- get.sum(ilog, "cdst")
  dst <- round(dst/1000,1)
  tim <- get.sum(ilog,"tim")
  min <- round(tim/30)/2
  sp <- " ";  comma <- ", "; crt <- "\n"; 
  c(dst,'K in ',min,' minutes</strong></p>',crt)
}
cat(hdr.dst(),sep="")

# Lines 4-5
hdr.img <- function(ilog=chk$ilog) {
  # <p><img src="log/log219.png" width="480" height="320"
  # alt="Map of November 17, 2012 run"></p>
  if (verbose) cat("v1.1.1 hdr.img\n")
  sp <- " ";  comma <- ", "; crt <- "\n"; 
  dat <- get.sum(ilog,"day")
  dd <- as.numeric(substr(dat,7,8))
  mm <- as.numeric(substr(dat,5,6))
  yyyy <- as.numeric(substr(dat,1,4))
  mon <- month.name[mm]
  c('<p><img src="log',ilog,'.png" width="480" height="320"',crt,
    'alt="Map of ',mon,sp,dd,comma,yyyy,' run"></p>',crt,crt)
}
cat(hdr.img(),sep="")

# Lines 6-7
hdr.ifi <- function(ilog=chk$ilog) {
  # I finished a total of 2.8 kilometers (1.7 miles)
  # in 21 1/2 minutes. This was 2 minutes slower than
  if (verbose) cat("v1.1.1 ifi.img\n")
  sp <- " ";  comma <- ", "; crt <- "\n"; 
  dst <- round(get.sum(ilog,"cdst")/1000,1)
  tim <- round(get.sum(ilog,"tim")/30)/2
  c('<p>I finished a total of ',dst,' kilometers ',crt,
    '(',round(mi(dst),1),' miles) in ',tim,' minutes. This is ',crt)
}
cat(hdr.ifi(),sep="")

# Lines 8-9
hdr.slo <- function(ilog=chk$ilog,r=chk$r) {
  # in 21 1/2 minutes. This was 2 minutes slower than
  # my best time on the
  # <a href="r143.html">143rd Street route</a>
  if (verbose) cat("v1.1.1 hdr.slo\n")
  # sp <- " ";  comma <- ", "; crt <- "\n"; 
  pac.bst(r,ilog)
  # ctm <- dtm[2]-dtm[1]
  # cmp <- 'the same speed as'
  # if (ctm < 0) {cmp <- c(abs(ctm),' minutes faster than')}
  # if (ctm > 0) {cmp <- c(ctm,     ' minutes slower than')}
  # c(cmp,' my best time on the ',crt,
  #  '<a href="',r,'.html">',r,' route</a> ',crt)
}
cat(hdr.slo(),sep="")

# Line 10
hdr.tra <- function(ilog=chk$ilog) {
  # and translates to a 38 minute 5K.  
  if (verbose) cat("v1.1.1 hdr.tra\n")
  sp <- " ";  comma <- ", "; crt <- "\n"; 
  dst <- get.sum(ilog,"cdst")
  tim <- get.sum(ilog,"tim")
  c(' and translates to a ',round(pace(dst/1000,tim/60)),' minute 5K.</p>',crt,crt) 
}
cat(hdr.tra(),sep="")

# Level 2

hdr.map <- function(ilog=chk$ilog,r=chk$r) {
  # <p><strong>Saturday, November 17<a name="r1117"></a>,
  # 4.7K in 35 minutes</strong></p>
  # <p><img src="log/log219.png" width="480" height="320"
  # alt="Map of November 17, 2012 run"></p>
  #
  # I finished a total of 2.8 kilometers (1.7 miles)
  # in 21 1/2 minutes. This was 2 minutes slower than
  # my best time on the
  # <a href="r143.html">143rd Street route</a>
  # and translates to a 38 minute 5K.  
  st <- c(hdr.dat(ilog),hdr.dst(ilog),hdr.img(ilog),hdr.ifi(ilog),hdr.slo(ilog,r),hdr.tra(ilog))
  paste(st,sep="",collapse="")
}
cat(hdr.map(),sep="")

# Level 3

hdr.web <- function(htm) {
  pth <- paste(def.path,"/pmean/personal/log/",sep="")
  fnm <- paste(pth,"log2016.html",sep="")
  fna <- paste(pth,"log2016a.html",sep="")
  log <- read.table(fnm,sep="~",quote="",as.is=TRUE,comment.char="")$V1
  write.table(log,file=fna,quote=FALSE,row.names=FALSE,col.names=FALSE)
  k0 <- grep("<!--begin insert-->",log,value=FALSE)
  k0 <- k0[1] # in case there are two "begin inserts"
  k2 <- length(log)
  vlist <- strsplit(htm,"\n")[[1]]
  new_log <- c(log[1:k0],vlist,log[(k0+1):k2])
  write.table(new_log,file=fnm,quote=FALSE,row.names=FALSE,col.names=FALSE)
  vlist
}

hdr.ins <- function(ilog=chk$ilog,r=chk$r) {
  # writes html code to log2014.html
  # uses functions hdr.map, hdr.web
  hdr.htm <- hdr.map(ilog,r)
  hdr.web(hdr.htm)
}

pac.beg <- function(r=chk$r,pix=320,xmar=0) {
  if (verbose) cat("v1.1.1 pac.beg\n")
  i.list <- pac.lst(r)
  map.ras <- get.ras(i.list[1])
  par(mar=rep(0,4),xaxs="i",yaxs="i")
  plot(c(-pix/2, xmar+pix/2),c(-pix/2, pix/2),type="n",axes=FALSE)
  rasterImage(map.ras, -pix/2, -pix/2, pix/2, pix/2)
  return(i.list)  
}
pac.beg()

pac.tin <- function(r=chk$r) {
  if (verbose) cat("v1.1.1 pac.tin\n")
  i.list <- pac.lst(r)
  if (length(i.list)==1) {i.list <- list(i.list)}
  pix.dat <- imp.pix(i.list)
  p <- pix.dat[pix.dat$i==i.list[1],]
  n <- dim(p)[1]
  segments(p[-n,"x"],p[-n,"y"],p[-1,"x"],p[-1,"y"],lwd=5,col="blue")
  t.min <- get.sum(i.list[1],"tim")
  mrk.dat <- matrix(-999,nrow=length(i.list),ncol=2)
  vnames <- c("x","y")
  dimnames(mrk.dat)[[1]] <- i.list
  dimnames(mrk.dat)[[2]] <- vnames
  for (i in as.character(i.list)) {
    pix.sub <- pix.dat[pix.dat$i==i,]
    mrk.dat[i,"x"]   <- round(cal.ing(pix.sub,"tim",t.min,"x"))
    mrk.dat[i,"y"]   <- round(cal.ing(pix.sub,"tim",t.min,"y"))
  }
  day <- pac.lst(r,"day")
  return(cbind(mrk.dat,day))
}
pac.tin()

pac.mrk <- function(r=chk$r) {
  if (verbose) cat("v1.1.1 pac.mrk\n")
  mrk.dat <- pac.tin(r)
  for (i in dimnames(mrk.dat)[[1]]) {
    map.cir(mrk.dat[i,"x"],mrk.dat[i,"y"],0,8," ",off.d=0,co="white")
  }
  text(mrk.dat[,"x"],mrk.dat[,"y"],1:dim(mrk.dat)[1],col="black")
  pos <- which(mrk.dat[,"day"]==max(mrk.dat[,"day"]))
  x <- mrk.dat[pos,"x"]
  y <- mrk.dat[pos,"y"]
  map.cir(x,y,0,8," ",off.d=0,co="black")
  text(x,y,pos,col="white")
  return(mrk.dat)
}
pac.beg()
pac.mrk()

pac.map <- function(r=chk$r) {
  pac.beg(r)
  pac.mrk(r)
}
pac.map()


pac.png <- function(r) {
  pth <- paste(def.path,"/pmean/personal/log/",sep="")
  full.filename <- paste(pth,r,".png",sep="")
  png(file=full.filename,width=320,height=320)
  pac.map(r)
  dev.off()
}

# Do not test this funtion willy nilly

hdr.sum <- function(r=chk$r) {
  # revises html code for page of all runs with a specific name
  # uses function hdr.pac
  pth <- paste(def.path,"/pmean/personal/log/",sep="")
  fnm <- paste(pth,r,".html",sep="")
  log <- read.table(fnm,sep="~",quote="",as.is=TRUE,comment.char="")$V1
  k0 <- grep("<!--begin insert-->",log,value=FALSE)
  k1 <- grep("<!--end insert-->",log,value=FALSE)
  k2 <- length(log)
  fna <- "archive/rbackup.html" # make a backup, just in case
  write.table(log,file=fna,quote=FALSE,row.names=FALSE,col.names=FALSE)
  vlist <- strsplit(hdr.pac(r),"\n")[[1]]
  new_log <- c(log[1:k0],vlist,log[k1:k2])
  write.table(new_log,file=fnm,quote=FALSE,row.names=FALSE,col.names=FALSE)
  return(c(k0,k1,k2))
}
# Don't test this function willy-nilly



hdr.pac <- function(r=chk$r) {
  # used by function hdr.sum
  library("chron")
  dat <- pac.lst(r,"day")
  tim <- pac.lst(r,"tim")
  dd <- as.numeric(substr(dat,7,8))
  mm <- as.numeric(substr(dat,5,6))
  yyyy <- as.numeric(substr(dat,1,4))
  mon <- month.name[mm]
  sp <- " "
  comma <- ", "
  s1 <- paste('<li><a href="log',yyyy,'.html#r',zpad(mm),zpad(dd),'">',sep="")
  s2 <- paste(mon,sp,dd,comma,yyyy,'</a>',comma,fmt.tim(tim),'</li>\n',sep="")
  s3 <- paste(s1,s2,sep="",collapse='')
  s4 <- paste("<ol>\n",s3,"</ol>\n",sep="",collapse="")
  return(s4)
  }
cat(hdr.pac())

cal.tot <- function() {
  run.dat <- get.sum(1:999)
  om  <- substr(run.dat$day,1,6)
  oy  <- substr(run.dat$day,1,4)
  od  <- substr(run.dat$day,1,3)
  omk <- round(tapply(run.dat$cdst,om,sum)/1000,1)
  oyk <- round(tapply(run.dat$cdst,oy,sum)/1000,1)
  odk <- round(tapply(run.dat$cdst,od,sum)/1000,1)
  omn <- tapply(run.dat$cdst,om,length)
  oyn <- tapply(run.dat$cdst,oy,length)
  odn <- tapply(run.dat$cdst,od,length)
  outside.runs <- data.frame(ymd=as.numeric(c(names(omk),names(oyk),names(odk))),o.cdst=c(omk,oyk,odk),o.runs=c(omn,oyn,odn))
  tmat <- read.csv("inftrd.csv")
  tm <- substr(tmat$ymd,1,6)
  ty <- substr(tmat$ymd,1,4)
  td <- substr(tmat$ymd,1,3)
  tmm <- round(tapply(tmat$dst,tm,sum),1)
  tym <- round(tapply(tmat$dst,ty,sum),1)
  tdm <- round(tapply(tmat$dst,td,sum),1)
  tmn <- tapply(tmat$dst,tm,length)
  tyn <- tapply(tmat$dst,ty,length)
  tdn <- tapply(tmat$dst,td,length)
  treadmill.runs <- cbind(ymd=as.numeric(c(names(tmm),names(tym),names(tdm))),t.dst=c(tmm,tym,tdm),t.runs=c(tmn,tyn,tdn))
  merge(outside.runs,treadmill.runs,all=TRUE)
}

#############
# bar chart #
#############

hdr.trd <- function(dat,mi,tm) {
  # <p><strong>Friday, December 27, 2013,
  # 0.4 miles in 10 minutes</strong>
  # <p><img src="tread131217.png" width="480" height="160"
  # alt="Bar chart of treadmill paces">
  library("chron")
  day.name <- c("Sunday", "Monday", "Tuesday",
                "Wednesday", "Thursday", "Friday", "Saturday")
  # dat <- run.dat[run.dat$ilog==ilog,"day"]
  dd <- as.numeric(substr(dat,5,6))
  mm <- as.numeric(substr(dat,3,4))
  yyyy <- 2000+as.numeric(substr(dat,1,2))
  mon <- month.name[mm]
  day <- day.name[1+day.of.week(mm,dd,yyyy)]
  dst <- mi
  tim <- tm
  min <- tim
  sp <- " "
  comma <- ", "
  st <- c(
    '<p><strong>',day,comma,mon,sp,dd,comma,yyyy,'\n',
    comma,dst,' miles in ',min,' minutes</strong></p>','\n',
    '<p><img src="tread',dat,'.png" width="480" height="160"','\n',
    'alt="Bar chart of treadmill paces"></p>','\n\n'
  ) 
  paste(st,sep="",collapse="")
}

treadbar <- function(n,tpat,xlab=1:length(tpat)) {
  # creates bar chart in png format
  xmax <- length(tpat)
  ymax <- max(tpat)
  fnm <- paste(def.path,"/pmean/personal/log/tread",n,".png",sep="")
  png(file=fnm,width=480,height=160)
  par(mar=c(4.1,3.6,0.6,0.1),las=2)
  barplot(tpat,width=0.8,space=0.25,axes=FALSE,xlab="Minute",)
  axis(side=2,at=0:ymax,labels=paste(0:ymax,"mph"))
  axis(side=1,at=0.6+0:(xmax-1),labels=xlab)
  dev.off()
  tmat <- read.csv("inftrd.csv")
  nrow <- dim(tmat)[1]
  dat4 <- 20000000+n
  tmat <- rbind(tmat,c(dat4,sum(tpat)/60,length(tpat)))
  print(tail(tmat))
  write.csv(tmat,"inftrd.csv",row.names=FALSE)
  html <- hdr.trd(n,sum(tpat)/60,sum(tpat>0))
  hdr.htm(html)
  tpat
}

cal.stop <- function(ilog=chk$ilog) {
  pix.dat <- get.pix(ilog)
  t.null <- which(diff(pix.dat[,"tim"])==0)
  if (length(t.null)>0) {pix.dat<- pix.dat[-(t.null+1),]}
  d.null <- which(diff(pix.dat[,"cdst"])==0)
  if (length(d.null)==0) {return("No stops")}
  d.null <- sort(unique(c(d.null,d.null+1)))
  return(pix.dat[d.null,])
}
i <- 550
cal.stop(i); i <- i-1

save.image(file="fun.RData")

