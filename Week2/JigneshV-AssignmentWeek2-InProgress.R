# Monty Hall Simulation


simulation<-function(Option='Default',strategy='switch',N=1000)
{
doors<-1:3

#Counter for win
win<-0

  for(i in 1:N)
  {
    #select one door as prize door
    prize<-sample(1:3,1)
   
    # guess one door
    guess<-sample(1:3,1)
   
    #INITIALIZE
    select<-0
    reveal<-0
    revealconter<-0
    #Reveal one door
    if(Option=="variation")
    {
      #Variation  : Reveal one door not guessed, but may or may not contain prize
      reveal<-sample(doors[-c(guess)],1)
    } else {
      #Default Approach  : Reveal one door not guessed and not having the prize.
      if(guess==prize)
      {
        reveal<-sample(doors[-c(prize)],1)
      } else {
        reveal<-doors[-c(prize,guess)]
      }
      
    }
    
    if(reveal==prize)
    {
      win<-win+1
    }else{
            if(strategy=="switch")
            {
              select<-doors[-c(reveal,guess)]
            }  else  {
              #strategy here is stay with orignal choice
              select<-guess
            }
            
            if(select==prize)
            {
              win<-win+1
            }
        
        }


}  #End of For loop
cat(paste('Summary: ',
          '\nStrategy: ',strategy,
          '\nOption: ',Option,
          '\nTotal Win: ',win,
          '\nTotal Attempts: ',N,
          '\nWin %: ',win/N,
          
          '\n'))
}

