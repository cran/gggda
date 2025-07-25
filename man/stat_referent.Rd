% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat-referent.r
\name{stat_referent}
\alias{stat_referent}
\alias{ggplot_add.LayerRef}
\title{Transformations with respect to reference data}
\usage{
stat_referent(
  mapping = NULL,
  data = NULL,
  geom = "blank",
  position = "identity",
  referent = NULL,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

\method{ggplot_add}{LayerRef}(object, plot, ...)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{geom}{The geometric object to use to display the data for this layer.
When using a \verb{stat_*()} function to construct a layer, the \code{geom} argument
can be used to override the default coupling between stats and geoms. The
\code{geom} argument accepts the following:
\itemize{
\item A \code{Geom} ggproto subclass, for example \code{GeomPoint}.
\item A string naming the geom. To give the geom as a string, strip the
function name of the \code{geom_} prefix. For example, to use \code{geom_point()},
give the geom as \code{"point"}.
\item For more information and other ways to specify the geom, see the
\link[ggplot2:layer_geoms]{layer geom} documentation.
}}

\item{position}{A position adjustment to use on the data for this layer. This
can be used in various ways, including to prevent overplotting and
improving the display. The \code{position} argument accepts the following:
\itemize{
\item The result of calling a position function, such as \code{position_jitter()}.
This method allows for passing extra arguments to the position.
\item A string naming the position adjustment. To give the position as a
string, strip the function name of the \code{position_} prefix. For example,
to use \code{position_jitter()}, give the position as \code{"jitter"}.
\item For more information and other ways to specify the position, see the
\link[ggplot2:layer_positions]{layer position} documentation.
}}

\item{referent}{The reference data set; see Details.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{...}{Additional arguments passed to \code{\link[ggplot2:layer]{ggplot2::layer()}}.}

\item{object}{An object to add to the plot}

\item{plot}{The ggplot object to add \code{object} to}
}
\value{
A \link[=gggda-ggproto]{ggproto} \link[ggplot2:layer]{layer}.
}
\description{
Compute statistics with respect to a reference data set with
shared positional variables.
}
\details{
Often in geometric data analysis a statistical transformation applied to data
\eqn{X} will also depend on data \eqn{Y}, for example when drawing the
projections of vectors \eqn{X} onto vectors \eqn{Y}. The stat layer
\code{stat_referent()} accepts \eqn{Y} as an argument to the \code{referent} parameter
and pre-processes them using the existing positional aesthetic mappings to
\code{x} and \code{y}.

The ggproto can be used as a parent to more elaborate statistical
transformations, or the stat can be paired with geoms that expect the
\code{referent} parameter and use it to position their transformations of \eqn{X}.
It pairs by default to \verb{[ggplot2::geom_blank()]} so as to prevent possibly
confusing output.
}
\examples{
# simplify the Motor Trends data to two predictors legible at aspect ratio 1
mtcars \%>\%
  transform(hp00 = hp/100) \%>\%
  subset(select = c(mpg, hp00, wt)) ->
  subcars
# compute the gradient of `mpg` against these two predictors
lm(mpg ~ hp00 + wt, subcars) \%>\%
  coefficients() \%>\%
  as.list() \%>\% as.data.frame() ->
  grad
# use the gradient as a reference (to no effect in this basic ggproto)
ggplot(subcars, aes(x = hp00, y = wt)) +
  coord_equal() +
  geom_point() +
  stat_referent(referent = grad)
ggplot(subcars, aes(x = hp00, y = wt)) +
  coord_equal() +
  stat_referent(geom = "point", referent = grad)
}
\concept{biplot layers}
