montyhallr = function(sims=1000,initial.door=1,wanna.switch="N"){
  
  if(initial.door %in% 1:3 & sims>0 & toupper(wanna.switch) %in% c("Y","N")){
  
  ###Create a 3x3 identity matrix
  ###0's are goats, 1's are cars
  ###each row is a unique arrangement of 2 goats, 1 car
  A = data.frame(diag(3))
  
  #initialize an empty vector
  my.result=c()
  
  #do the simulations
  for(i in 1:sims){
    
    #generate a random uniform variable between 1 and 3
    sequence=floor(runif(1,min=1,max=4))
    
    #Randomly select a sequence of 2 goats and 1 car
    #i.e. randomly select a row from the 3x3 identity matrix
    if(sequence==1){key=A[1]} #chooses the 1,0,0 sequence
    else if(sequence==2){key=A[2]} #chooses the 0,1,0 sequence
    else if(sequence==3){key=A[3]} #chooses the 0,0,1 sequence
    
    ###Result if you want to stay
    ###It doesn't really matter what happens with the other doors if you always stay with your initial door

    if(toupper(wanna.switch)=="N"){
      my.result[i]=key[initial.door,]
    }
    
    ####Result if you want to switch
    else if(toupper(wanna.switch)=="Y"){
    
    #Subset to only doors you didn't initially choose
    if(initial.door==1){other.doors=key[2:3,]}
    if(initial.door==2){other.doors=key[c(1,3),]}
    if(initial.door==3){other.doors=key[1:2,]}
    
    #####Lets remove a goat!
    #####i.e. remove one of the doors that is 0 among the doors you didn't initially choose
    
    #if two elements are 0, remove one
    #arbitrarily remove the first 0
    if(other.doors[1]==0 & other.doors[2]==0){
      my.result[i]=other.doors[-1]
    }
    
    #if only element one is 0, remove it
    if(other.doors[1]==0 & other.doors[2]==1){
      my.result[i]=other.doors[-1]
    }
    
    #if only element two is 0, remove it
    if(other.doors[1]==1 & other.doors[2]==0){
      my.result[i]=other.doors[-2]
    }
  }
  }
  print(paste0("You chose door ",initial.door))
  print(paste0("Did you switch?: ",toupper(wanna.switch)))
  print(paste0("Simulations ",sims))
  print(paste0("Car win percentage: ",round(100*sum(my.result)/sims,5),"%"   ))
  }
  else{
    print("Check your params, homie")
    
  }
  }
  
  montyhallr(sims=10000,initial.door=1,wanna.switch="N")
  montyhallr(sims=10222,initial.door=2,wanna.switch="Y")
  montyhallr()
  montyhallr(sims=334,initial.door=3,wanna.switch="Y")
  montyhallr(sims=80082,initial.door=2,wanna.switch="Y")
  