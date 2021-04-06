# Linear Regression

A from scratch implementation of a linear regression algorithm in Haskell.

Implements the building blocks required to:

* Compute the slope and intercept of a line given two or more points
* Compute the equation of a straight line given two points for a perfect line (`y=mx+b`) in order to calculate the dependent variable (y) from an independent variable (x)
* Estimate the equation of a straight line given a set of points (`y=mx+b`) in order to estimate the dependent variable (y) from an independent variable (x)

Uses Mean Square Error as the cost function and gradient decent in order to minimise these errors to compute the best fit line.

## Running

```bash
stack install
stack run
```