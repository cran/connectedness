% connect.Rd
%--------------------------------------------------------------------------
% What: Example dataset
% $Id$
% Time-stamp: <2006-04-30 16:32:53 ggorjan>
%--------------------------------------------------------------------------

\name{connect}

\alias{connect}

\docType{data}

\title{Example data}

\description{
  Example data with two disconnected sets taken from Fernando et
  al. (1983) with added NA's to test robustnes of the code.   
}


\usage{data(connect)}

\format{
  A data frame with 10 observations on the following variables.
  \describe{
    \item{group}{character, group level}
    \item{season}{integer, season level}
  }
}

\references{
  Fernando, R. L. and Gianola, D. and Grossman, M. (1983) Identify all
  connected subsets in a two-way classification without interaction.
  Journal of Dairy Science, 66: 1399-1402
}

\keyword{datasets}

%--------------------------------------------------------------------------
% connect.Rd ends here
