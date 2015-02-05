#This function will read 

init <- function(){
  library("RCurl") 
  library("XML")
  library("bitops")
  source("zillow.R")
  source("zillowErrors.R")
  source("comps.R")

  csv <<- read.csv("textexport.csv")
  csvsub <<- csv[1:10,]
  zid <<- "X1-ZWz1d90oa09zpn_7nbg7"
  Insurance <<- 50
  InterestRate <<- 4.5
  MortageLength <<- 30
  AmountDown <<- .25
  
}

mortagePayment <- function(P=500000, I=6, L=30) { 
  J <- I/(12 * 100)
  N <- 12 * L
  M <- P*J/(1-(1+J)^(-N))
  return(M)
}

mortgage <- function(P=500000, I=6, L=30, amort=T, plotData=T) { 
  J <- I/(12 * 100)
  N <- 12 * L
  M <- P*J/(1-(1+J)^(-N))
  monthPay <<- M
  cat("\nThe payments for this loan are:\n 
      Monthly payment: $", M, " (stored in monthPay)\n
      Total cost: $", M*N, "\n\n", sep="")
  # Calculate Amortization for each Month
  if(amort==T) {
    Pt <- P # current principal or amount of the loan
    currP <- NULL
    while(Pt>=0) {
      H <- Pt * J # this is the current monthly interest
      C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
      Q <- Pt - C # this is the new balance of your principal of your loan
      Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
      currP <- c(currP, Pt)
    }
    monthP <- c(P, currP[1:(length(currP)-1)])-currP
    aDFmonth <<- data.frame(
      Amortization=c(P, currP[1:(length(currP)-1)]), 
      Monthly_Payment=monthP+c((monthPay-monthP)[1:(length(monthP)-1)],0),
      Monthly_Principal=monthP, 
      Monthly_Interest=c((monthPay-monthP)[1:(length(monthP)-1)],0), 
      Year=sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
    )
    aDFyear <- data.frame(
      Amortization=tapply(aDFmonth$Amortization, aDFmonth$Year, max), 
      Annual_Payment=tapply(aDFmonth$Monthly_Payment, aDFmonth$Year, sum), 
      Annual_Principal=tapply(aDFmonth$Monthly_Principal, aDFmonth$Year, sum), 
      Annual_Interest=tapply(aDFmonth$Monthly_Interest, aDFmonth$Year, sum), 
      Year=as.vector(na.omit(unique(aDFmonth$Year)))
    )
    aDFyear <<- aDFyear
    cat("The amortization data for each of the", N, "months are stored in \"aDFmonth\".\n\n")
    cat("The amortization data for each of the", L, "years are stored in \"aDFyear\".\n\n")
  }
  if(plotData==T) {
    barplot(t(aDFyear[,c(3,4)]), 
            col=c("blue", "red"), 
            main="Annual Interest and Principal Payments", 
            sub="The data for this plot is stored in aDFyear.",
            xlab="Years", ylab="$ Amount", 
            legend.text=c("Principal", "Interest"), 
            ylim=c(0, max(aDFyear$Annual_Payment)*1.3))
  }
}


getEstimate <- function(rownum,csvi){
  
  address = paste(csvi[rownum,]$House.Number, csvi[rownum,]$Compass, csvi[rownum,]$Street.Name, csvi[rownum,]$St.Suffix, csvi[rownum,]$Unit.., sep=' ')
  city = csv[rownum,]$City.Town.Code
  estimate = zestimate(address,city,zid) 
  return(estimate$rentamount)
}

getCashflow <- function(rownum,csvi){
  RentalAmount <- csvi$rentamount
  TaxAmount <- csvsub$Taxes
  ListPrice <- csvsub$List.Price
  MortageAmount <- mortagePayment(ListPrice - (ListPrice * AmountDown), I=InterestRate, L=MortgageLength)
  
  #Cashflow <- RentalAmount <- (MortgageAmount + TaxAmount + Insurance + HOA)
  
}


init()

re <- sapply(1:nrow(csvsub),getEstimate,csvsub)
csvsub$rentamount <- re
csvsub$rentamount