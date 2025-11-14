breed_transformation <- function(fish, time_step){
#aging fish
fish <- NLset(turtles = fish, agents = fish, var = "age", val = fish$age+(time_step/365))
####2. Transform########

transform.eggs=NLwith(fish, var="breed", val="eggs")
transform.eggs<- transform.eggs[transform.eggs$age>0.01]

if(nrow(transform.eggs)>1) fish <- NLset(turtles=fish, agents=turtle(fish, who=transform.eggs$who), var="breed", val="larvae")

#transform larvae to juveniles
transform.larvae=NLwith(fish, var="breed", val="larvae")
transform.larvae<- transform.larvae[transform.larvae$length>3]

if(nrow(transform.larvae)>1) fish <- NLset(turtles=fish, agents=turtle(fish, who=transform.larvae$who), var="breed", val="juvenile")

#transform juveniles to adult (only if doing a multiyear projection)
transform.juv=NLwith(fish, var="breed", val="juvenile")
transform.juv<- transform.juv[transform.juv$age>=1]

if(nrow(transform.juv)>1) fish <- NLset(turtles=fish, agents=turtle(fish, who=transform.juv$who), var="breed", val="adult")

return(fish)
}

