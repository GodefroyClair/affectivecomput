
cifer <- "gluhtlishjrvbadvyyplkaohavbyjpwolypzavvdlhrvuuleatlzzhnlzdpajoavcpnlulyljpwolyrlfdvykpzaolopkkluzftivsvmklhaoputfmhcvypalovsilpulukvwduw" 
cifer2 <- "l judee hghyhubwklqv l frxog ilqg sohdvh uhwxuq dqb eoxh sulqwv iru ydxow dqg dodup ghvljq edvgrqrqzkif edqn brx ghflgh rq l dp vhwwlqj svdih krxvh fr"
messa2 <- "i grabb edeverythins i could find please return any blue prints for vault and alarm design basdononwhfc bank you decide on i am setting psafe house co"
messa1 <- "ze name blackout worried that our cipher is too weak on next message switch to vigenere cipher keyword is the hidden symbol of death in my favorite holbein end"
cifer.array <- unlist(strsplit(cifer, ""))
cifer2.array <- unlist(strsplit(cifer2, ""))
table(cifer.array)
lookup <- c(l = "e", m = "f", n = "g", o = "h", p = "i", q = "j", r = "k",
           s = "l", t = "m", u = "n", v = "o", w = "p", x = "q", y = "r",
           z = "s", a = "t", b = "u", c = "v", d = "w", e = "x", f = "y",
           g = "z", h = "a", i = "b", j = "c", k = "d")
    table(cifer.array)
lookup2 <- c(a = "x", b = "y", c = "z", d = "a", e = "b", f = "c",
            g = "d", h = "e", i = "f", j = "g", k = "h", l = "i", 
            m = "j", n = "k", o = "l", p = "m", q = "n", r = "o",
            s = "p", t = "q", u = "r", v = "s", w = "t", x = "u", y = "v",
            z = "w")
paste(unname(lookup[cifer.array]), collapse = "")
paste(unname(lookup2[cifer2.array]), collapse = "")

ciph <- "Klkbnqlcytfysryucocphgbdizzfcmjwkuchzyeswfogmmetwwossdchrzyldsbwnydednzwnefydthtddbojice"
ciph2 <- "mlucdygicczhoadrzcylwadsxpilpiecskomoltejtkmqqymehpmmjxyolwpeewjckznpccpsvsxauyodhalmrioc"
ciph3 <- "wpelwbcniyfxmwjcemcyrazdqlsomdbfljwnbijxpddsyoehxpceswtoxwbleecsaxcnuetzywfn"
pass <- "crane"

charToRaw(ciph)
