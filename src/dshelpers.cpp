#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
CharacterVector bychanges(CharacterVector dat) {
   
   int n = dat.length();
   CharacterVector res(1);
   //int counter = 0;
   
   res[0] = dat[0];
   
   for(int i = 1; i < n; i++) {
      
      if (dat[i] != dat[i - 1]) {
         res.push_back(dat[i]);
      }
      
   }
   
   return res;
}


// [[Rcpp::export]]
LogicalVector byfirst(CharacterVector dat) {

 int n = dat.length();
 LogicalVector res(n);


 res[0] = true;

 for(int i = 1; i < n; i++) {

   if (dat[i] == dat[i - 1]) {
     res[i] = false;
   } else {

     res[i] = true;
   }

 }

 return res;
}


// [[Rcpp::export]]
LogicalVector bylast(CharacterVector dat) {
   
   int n = dat.length() - 1;
   LogicalVector res(n + 1);
   
   res[n] = true;
   
   for(int i = 0; i < n; i++) {
      
      if (dat[i] == dat[i + 1]) {
         res[i] = false;
      } else {
         
         res[i] = true;
      }
      
   }
   
   return res;
}


// [[Rcpp::export]]
RawVector getsigs(RawVector dat) {
  
  int n = dat.length();
  int spos;
  int npos;
  
  spos = dat[0];
  
  for(int i = 1; i < n; i++) {
    
    npos = dat[i];

    spos = spos ^ npos;

  }
  
  RawVector ret(1);
   
  ret[0] = spos;
  
  return ret;
}

// getBitSignature <- function(x) {
//   
//   spos <- x[1]
//   for (i in seq(2, length(x))) {
//     
//     spos <- xor(spos, x[i]) 
//   }
//   
//   return(spos)
// }

// captureSignatures <- function(dat) {
//   
//   ret <- list()
//   
//   att <- attributes(dat)
//   for (nm in names(att)) {
//     if (!nm %in% c("class", "name"))
//       attr(dat, nm) <- NULL
//   }
//   
//   idat <- serialize(dat, connection = NULL)
//     
//     ret$Length <- length(idat)
//     ret$Hex <- getBitSignature(idat)
//     
//     return(ret)
// }




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R


#bychanges(as.character(mtcars[["gear"]]))

#inp <- mtcars[order(mtcars[["cyl"]]), ]

#dat <- add_autos(inp, c("cyl"))

#dat

#paste(mtcars[["am"]], mtcars[["gear"]])

#dat["first."] <- byfirst2(mtcars, c("am", "gear"))

#dat["first."] <- byfirst(mtcars, "am")
#dat["last."] <- bylast(mtcars, c("am"))

#dat

# 
# nrow(mtcars)
# ncol(mtcars)


*/
