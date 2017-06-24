#' Test
#' 
#' Test
#' 
#' @param n
#' @export
#' 
fts.rar = function(n=100, 
d=11, Psi = NULL, op.norms = NULL, burnin=20, noise="mnorm", sigma=diag(d:1)/d, df=4)
{
  
  if(!is.null(Psi) && d!=dim(Psi)[1])
	stop("d must be equal to the dimension of Psi")

  if(!is.null(Psi) && dim(Psi)[1]!=dim(Psi)[2])
	stop("coefficients need to be square matrices")

  if(!is.null(Psi))
	d=dim(Psi)[1]

  if(d %% 2==0)
	stop("d must be odd")
  
  if (is.null(Psi)){
		Psi = exp(-(1:d))%*%t(exp(-(1:d)))
		Psi = Psi/norm(Psi,type='F')/2
	}  	

  if(is.matrix(Psi)){
  	w=array(0,c(dim(Psi)[1],dim(Psi)[2],1))	
  	w[,,1]=Psi
  	Psi=w
  }	
  
  p=dim(Psi)[3]

  if(is.null(op.norms)){
  op.norms=c()
  for(i in 1:p){op.norms=c(op.norms,norm(Psi[,,i]))}
  }

  for(i in 1:p){
 	Psi[,,i]=Psi[,,i]/norm(Psi[,,i],type="f")*op.norms[i]
  }	
  
  arg <- list()
  arg[['n']] <- n
  arg[['Psi']] <- Psi
  arg[['burnin']] <-burnin
  arg[['noise']] <- noise
  arg[['sigma']] <- sigma	
  arg[['df']] <- df
  X=do.call(rar, arg)

  basis=create.fourier.basis(nbasis=d)
  fd(t(X),basis)
}

