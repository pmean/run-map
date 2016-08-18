# amap 

rm(list=ls())
load("../../null/basic.RData")
load("fun.RData")

ilog <- 571; r <- "medium";      rty="Mission"; nt<-"66F and sunny. Dog reluctant at first. Alternated fast/slow minutes. Sore calves, no joint pains."
ilog <- 572; r <- "block";       rty="Mission"; nt<-"55F and cloudy. New shoes. Numb middle toes on right and ankle pain on left."
ilog <- 573; r <- "short";       rty="Mission"; nt<-"45F and sunny. Alternated fast/slow minutes. Right shoe still a bit tight, but better. No pains."
ilog <- 574; r <- "short";       rty="Mission"; nt<-"59F and sunny. Slow, steady pace. Left ankle pain and numb right toes."
ilog <- 575; r <- "mini";        rty="Mission"; nt<-"45F and rain. Wore running jacket and shorts. Dog was fine. Tried fast pace. Left heel pain."
ilog <- 576; r <- "cliff";       rty="Timed8K"; nt<-"54F and sunny. Dizzy at 2K, untied shoe at 3.8K. No joint pains."
ilog <- 577; r <- "r143";        rty="Mission"; nt<-"48F at dawn. Slow steady pace. New shoes starting to fit better."
ilog <- 578; r <- "block";       rty="Mission"; nt<-"65F, humid, after sunset. Traffic problems at 143rd near Nall. Same shoe problems."
ilog <- 579; r <- "church2";     rty="Mission"; nt<-"41F. Dawn run. Old shoes. No aches or joint pains."
ilog <- 580; r <- "large";       rty="Mission"; nt<-"34F and morning sun. Dog puked at 3K. Old shoes, no aches or joint pains."
ilog <- 581; r <- "mini";        rty="Mission"; nt<-"63F and cloudy. Storm threatened but never came. New shoes again but only minor problems."
ilog <- 582; r <- "church2";     rty="Mission"; nt<-"43F at start, 50F at end. Slow steady pace. Old shoes. Sore back calves but no joint pains."
ilog <- 583; r <- "double";      rty="Mission"; nt<-"57F and sunny. No dog per vets advice. Alternated fast/slow minutes. New shoes starting to break in."
ilog <- 584; r <- "church2";     rty="Mission"; nt<-"33F and blustery. New shoes slowly getting better. Slow steady pace."
ilog <- 585; r <- "r143";        rty="Mission"; nt<-"54F, night run. Sore left heel and right knee during warmup. Steady fast pace."
ilog <- 586; r <- "Sprint5K";    rty="Timed5K"; nt<-"61F and drizzle. Sore calves at start, very sore right back thigh at 3K. Coasted last 2K. 38:19.9."
ilog <- 587; r <- "mini";        rty="Mission"; nt<-"32F and rainy. No ice. Aches everywhere, biggest was left back thigh."
ilog <- 588; r <- "bell";        rty="Mission"; nt<-"34F, clouds, light wind. Minor left heel ache only problem. Tried running fast up, slow down."
ilog <- 589; r <- "mediumx";     rty="Mission"; nt<-"55F and sunny. Both heels sore during warmup. Alternated fast/slow. Back left thigh cramp."
ilog <- 590; r <- "medium";      rty="Mission"; nt<-"54F and sunny. Sore back left thigh during warmup. Sore calves late. Very slow pace."
ilog <- 591; r <- "short";       rty="Mission"; nt<-"55F, cloudy, light wind. Right knee and ankle twinges during warmup. Sore back left thigh."
ilog <- 592; r <- "short";       rty="Mission"; nt<-"57F, sunny, light wind. Sore muscles above and below both knees. Back left calf hurts a bit less."
ilog <- 593; r <- "medium";      rty="Mission"; nt<-"46F, cloudy, no wind. Right ankle pain during warmup. Only very minor muscle aches."
ilog <- 594; r <- "mini";        rty="Mission"; nt<-"54F, sunny, light wind. Minor aches everywhere but nothing major."
ilog <- 595; r <- "medium";      rty="Mission"; nt<-"36F,sunny, no wind. No gloves needed. Sore calves at 2.5K, but no joint pain."
ilog <- 596; r <- "short";       rty="Mission"; nt<-"36F, cloudy, light wind. Long sleeve shirt, shorts, no gloves. Left back thigh sore at 5 minutes."
ilog <- 597; r <- "big";         rty="Mission"; nt<-"37F, sunny, no wind. Took off gloves at 13 min. No notable aches or pains."
ilog <- 598; r <- "short";       rty="Mission"; nt<-"No notes."
ilog <- 599; r <- "NewYear5K";   rty="Timed5K"; nt<-"27F, light wind, sunny. Untied shoe at 16 minutes. No major aches or pains."
ilog <- 600; r <- "r143";        rty="Mission"; nt<-"21F, sunny, but windy, especially on return. Calf aches at 6 and 11 minutes."
ilog <- 601; r <- "church2";     rty="Mission"; nt<-"23F, cloudy, cold NW wind. Numb right toes at 30 minutes. No aches or pains."
ilog <- 602; r <- "short";       rty="Mission"; nt<-"30F and windy. One layer, took off gloves at 12 minutes. Sore right foot during warmup."
ilog <- 603; r <- "medium";      rty="Mission"; nt<-"57F and sunny. Fast and steady for first 2K, then slower. Left heel pain at 2K"
ilog <- 604; r <- "r143";        rty="Mission"; nt<-"39F, cloudy, calm. Running jacket and shorts. No aches or joint pains."
ilog <- 605; r <- "church2";     rty="Mission"; nt<-"34F, sunny, calm. Alternated fast/slow minutes. Gloves until 11 minutes. No pains."
ilog <- 606; r <- "r143";        rty="Mission"; nt<-"45F, breezy, morning sun. Running jacket and pants. Sore muscles at 5 min."
ilog <- 607; r <- "SanDiego-3";  rty="Outside"; nt<-"63F, light wind, after sunset. Tight right toes at 32 min. No major aches or pains."
ilog <- 608; r <- "SanDiego-4";  rty="Outside"; nt<-"63F, calm evening. Sore muscles at start. Untied shoe at 6 min."
ilog <- 609; r <- "short";       rty="Mission"; nt<-"48F, sunny, windy. Sinus pain during warmup. Stopped for ambulance at 25 minutes."
ilog <- 610; r <- "mini";        rty="Mission"; nt<-"55F, sunny, light wind. New shoes. Sore ankles during warmup."
ilog <- 611; r <- "r143";        rty="Mission"; nt<-"46F, light wind, dawn. Shorts and tshirt. Brief right ankle pain."
ilog <- 612; r <- "roe";         rty="Mission"; nt<-"57F, cloudy, light wind. Tight toes, but no aches or pains."
ilog <- 613; r <- "stpat4M";     rty="Timed4M"; nt<-"52F, cloudy. Tight toes at 30 minutes. Sore knees after finish."
ilog <- 614; r <- "miami2";      rty="Outside"; nt<-"75F, cloudy, humid. No aches or pains."
ilog <- 615; r <- "miami3";      rty="Outside"; nt<-"72F, humid, pre-dawn run. Padded little toes. No aches or pains."
ilog <- 617; r <- "keywest1";    rty="Outside"; nt<-"81F, sunny, muggy. Several wrong turns. No aches or pains."
ilog <- 618; r <- "mini";        rty="Mission"; nt<-"61F, sunny, windy. Sore calf muscles early, dizzy at 23 minutes. Old shoes, no joint pains."
ilog <- 619; r <- "medium";      rty="Mission"; nt<-"63F, cloudy, very windy. Alternated fast/slow. New shoes were fine. No joint pains. "
ilog <- 620; r <- "r143";        rty="Mission"; nt<-"54F, light wind, just after sunset. Very stiff at start. Alternated fast/slow minutes."
ilog <- 621; r <- "block";       rty="Mission"; nt<-"52F, late afternoon sun, light wind. Alternated fast/slow minutes. No aches or pains."
ilog <- 622; r <- "short";       rty="Mission"; nt<-"57F, cloudy, light wind. Slow and steady. Calves very sore at 11 minutes."
ilog <- 623; r <- "Chicago2";    rty="Outside"; nt<-"30F at dawn. Only shorts and t. Very cold until 15 min. Stairs at 2 min, untied shoe at 5 min."
ilog <- 624; r <- "r143";        rty="Mission"; nt<-"43F, sunny. Alternated fast/slow. No aches or pains. Dizzy at 18 min."
ilog <- 625; r <- "mini";        rty="Mission"; nt<-"63F, sunny, very strong wind from west. Dizzy at 23 min. Alternated fast/slow."
ilog <- 626; r <- "block";       rty="Mission"; nt<-"42F, sunny. Running jacket/pants, no gloves. Alternated fast/slow. No aches or pains."
ilog <- 627; r <- "short";       rty="Mission"; nt<-"54F, sunny, windy. Alternated fast/slow. Sore lower calves at 15 min."
ilog <- 628; r <- "r151";        rty="Mission"; nt<-"54F, sunny. Ran all out for as long as I could. No aches or pains."
ilog <- 629; r <- "bellcircle";  rty="Mission"; nt<-"bellcircle 55F, cloudy. Slow and steady. Tight toes at 34 min, sore right hip at 42 min."
ilog <- 630; r <- "r143";        rty="Mission"; nt<-"61F, drizzle then light rain. Slow and steady. Sore muscles but no joint pains."

ilog <- 631; r <- "mini";        rty="Mission"; nt<-"64F, sunny. Alternated fast/slow. High right ankle pain. Dizzy at end."
ilog <- 632; r <- "block";       rty="Mission"; nt<-"48F, sunny. Very slow pace to stretch and work out kinks."
ilog <- 633; r <- "trolley";     rty="Timed4M"; nt<-"63F, sunny. Sore ankles at start, dizzy near finish."
ilog <- 634; r <- "mini";        rty="Mission"; nt<-"63F, rain but mostly after run. Left ankle pain during warmup, pain in both heels early."
ilog <- 635; r <- "short";       rty="Mission"; nt<-"55F, sunny. Sore left heel during warmup. Right thigh cramp at 2K."
ilog <- 636; r <- "block";       rty="Mission"; nt<-"61F, partly sunny, humid. Tight toes at 24 min. Right thigh cramp at 5 min. Sore left heel."
ilog <- 637; r <- "r143";        rty="Mission"; nt<-"45F, dawn run. Running jacket and shorts. Alternated fast/slow. Slight twinge in left heel."
ilog <- 638; r <- "block";       rty="Mission"; nt<-"48F, dawn run. Block. Alternated fast/slow. T shirt. Stiff left ankle during warmup, sore at 20 min."
ilog <- 639; r <- "medium";      rty="Mission"; nt<-"63F, sunny. Medium. Moderate pace. Sore left ankle during warmup, twinge in right ankle at 3 min."

ilog <- 641; r <- "short";       rty="Mission"; nt<-"54F, partly sunny. Small loop. Very slow. Sore left ankle, also right ankle and knee, left hip."
ilog <- 642; r <- "r143";        rty="Mission"; nt<-"Rain and thunder"
ilog <- 643; r <- "block";       rty="Mission"; nt<-"63F, partly cloudy, humid. Block. Sore left ankle early, sore right knee, achy thigh late."
ilog <- 644; r <- "Loose8K";     rty="Timed8K"; nt<-"72F, sunny. Very sore ankles during warmup. Tight right toes at 3K, sore high left ankle at 4K."
ilog <- 645; r <- "r143";        rty="Mission"; nt<-"64F, dawn run. R143. Sore calves during warmup, right hip pain at 2K."
ilog <- 646; r <- "sixdot";      rty="Timed6K"; nt<-"73F, cloudy, humid. Left GPS on extra min. Untied shoe before 1 min, rock in shoe late. No aches."
ilog <- 647; r <- "r143";        rty="Mission"; nt<-"70F, early sun. No headphones. R143. No aches or pains."

ilog <- 648; r <- "block";       rty="Mission"; nt<-"70F, early sun. No headphones. R143. No aches or pains."
ilog <- 650; r <- "r143";        rty="Mission"; nt<-"70F, early sun. No headphones. R143. No aches or pains."
ilog <- 651; r <- "block";       rty="Mission"; nt<-"70F, early sun. No headphones. R143. No aches or pains."
ilog <- 652; r <- "Ward4M";      rty="Timed4M"; nt<-"70F, early sun. No headphones. R143. No aches or pains."

ilog <- 653; r <- "medium";      rty="Mission"; nt<-"75F, sunny. Very slow pace. Tired but no aches or pains."
ilog <- 654; r <- "r143";        rty="Mission"; nt<-"81F, sunny. R143. No joint pains."

ilog <- 655; r <- "medium";      rty="Mission"; nt<-"73F and sunny. Medium. No joint pains."
ilog <- 656; r <- "r143";        rty="Mission"; nt<-"75F, humid, dawn run. Sore thigh muscles at 8 minutes."
ilog <- 657; r <- "block";       rty="Mission"; nt<-"79F, dawn run, humid. Block. No aches or pains."

setInternet2(use = FALSE)
verbose <- FALSE
def.path <- "c:/a01/u"
def.path <- "I:/u"              

all.races <- get.sum(1:999); tail(all.races)

imp.all(ilog, r, rty, nt)

chk1 <- map.png(ilog)
chk4 <- hdr.ins(ilog,r)

chk5 <- pac.png(r)
chk6 <- hdr.sum(r)

sp <- cal.bar(ilog)

tail(cal.tot())
head(cal.tot(),7)

# The formula was originally devised by Pete Riegel,
# a research engineer and marathoner, and published
# in Runner's World many moons ago by Owen Anderson
# in 1997. It has stood the test of time since then
# and has been widely used.
#
# The formula is T2 = T1 x (D2/D1)1.06 where T1 is
# the given time, D1 is the given distance, D2 is
# the distance to predict a time for, and T2 is
# the calculated time for D2. 


tread131230 <- treadbar(131230,c(1,2,3,3,3,4,5,5,5,5,5,5,5,5,4,3,3,3,2,1))

#            0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 
#            1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4

tread01 <- c(3,3,3,3,3,4,5,4,5,4,5,4,5,4,5,4,5,3,3,3,3,3)
tread02 <- c(3,3,3,3,3,4,5,6,5,4,5,6,5,4,5,6,5,4,3,3,3,3,3)
tread03 <- c(2,3,3,3,3,4,6,4,6,4,6,4,6,4,6,4,6,4,3,3,3,3,2)
tread04 <- c(2,3,3,3,3,4,7,4,7,4,7,4,7,4,7,4,7,4,3,3,3,3,2)
tread05 <- c(2,3,3,3,3,4,7,7,4,4,7,7,4,4,7,7,4,3,3,3,3,2)
tread06 <- c(2,3,3,3,3,4,7,7,4,4,7,7,4,4,7,7)
tread07 <- c(2,3,3,3,3,4,7,7,4,4,7,7,4,4,4,7,4,4,3,3,3,3,2)
tread08 <- c(2,3,3,3,3,4,7,7,7,4,4,4,4,7,7,7,4,3,3,3,3,2)
tread09 <- c(2,3,3,3,3,4,5,5,5,5,5,5,5,5,5,5,5,5,4,3,3,3,3,2)
tread10 <- c(2,3,3,3,3,3,3,3,3,2)
tread11 <- c(2,2,3,3,3,3,3,3,2)
tread12 <- c(2,3,3,3,3,3,2)
tread13 <- c(2,2,3,3,3,3,2,2)
tread14 <- c(2,2,3,3,3,4,7,7,4,4,7,7,4,4,7,7,4,3,3,3,2,2)
tread15 <- c(2,3,3,3,3,4,7,7,7,4,4,7,7,4,4,7,7,4,4,3,3,3,2,2)
tread16 <- c(2,2,3,3,3,4,5,5,5,5,5,5,4,3,3,3,2,2)
tread17 <- c(2,2,3,3,3,4,7,7,7,4,4,7,7,4,4,3,3,3,2,2)
tread18 <- c(2,2,3,3,3,4,7,0,4,7,7,4,4,7,7,4,4,7,7,7,4,3,0,3,3,2,2)
tread19 <- c(2,2,3,3,3,3,3,3,2,2)
tread20 <- c(2,2,3,3,3,4,7,7,7,4,4,7,7,4,4,7,7,7,4,3,3,3,2,2)
tread21 <- treadbar(21,c(2,2,2,3,3,3,3,3,3,2,2,2))
tread22 <- treadbar(22,c(1,2,3,3,3,3,3,3,3,3,2,1))
tread23 <- treadbar(23,c(1,2,2,2,2,2,2,2,2,1))
tread24 <- treadbar(24,c(1,2,3,3,3,4,7,7,4,4,7,7,4,4,7,7,4,4,7,7,4,3,3,3,2,1))
tread25 <- treadbar(25,c(1,2,3,3,3,3,3,3,2,1))
tread26 <- treadbar(26,c(1,2,3,3,3,4,5,5,5,5,5,5,5,5,4,3,3,3,2,1))
tread131217 <- treadbar(131217,c(1,2,3,3,3,3,3,3,2,1))
tread131215 <- treadbar(131215,c(1,2,3,3,3,4,7,4,7,4,7,4,7,4,7,4,7,4,7,4,7,4,7,4,7,4,3,3,3,2,1))
tread131213 <- treadbar(131213,c(1,2,3,3,3,4,7,7,4,4,7,7,4,4,7,7,4,3,3,3,2,1))
tread131226 <- treadbar(131226,c(1,2,3,3,3,4,7,7,4,4,7,7,4,4,7,7,4,4,7,7,4,4,7,7,4,4,7,7,4,3,3,3,2,1))
tread131228 <- treadbar(131228,c(1,2,2,3,4,4,5,5,5,5,5,5,5,5,5,5,5,0,1,1,1,1))
tread131230 <- treadbar(131230,c(1,2,3,3,3,4,5,5,5,5,5,5,5,5,4,3,3,3,2,1))
                                 
warm.up.seg <- c(1,2,3,3,3,4)
cool.off.sg <- c(4,3,3,3,2,1)

tread140104 <- treadbar(140104,c(warm.up.seg,5,5,5,5,5,5,5,5,cool.off.sg))
tread140106 <- treadbar(140106,c(warm.up.seg,5,5,5,5,5,5,5,5,cool.off.sg))
tread140109 <- treadbar(140109,c(warm.up.seg,7,7,4,4,7,7,4,4,7,7,cool.off.sg))
tread141115 <- treadbar(141115,c(warm.up.seg,5,5,cool.off.sg))
tread141117 <- treadbar(141117,c(warm.up.seg,5,5,5,5,5,5,5,5,cool.off.sg))

#                                0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3
#                                1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5

