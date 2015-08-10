function result = ols(y,x)
%
% Does ols. You need to give it a constant if you want it.
%
% result = ols(y,x)
%
% y is N x 1
% x is N x M

ybar = mean(y);
n = length(y);
k = size(x,2);

% Perform the regression.
b = (x' * x)^(-1) * x' * y;
yhat = x * b;
ehat = y - yhat;

varE = 1/(n-k) * (ehat' * ehat);
varB = varE * (x' * x)^(-1);

% Get some stats.
result.y = y;
result.x = x;
result.b = b;
result.t = result.b./sqrt(diag(varB)); 
result.serr = sqrt(diag(varB));
result.p = (1-tcdf(result.t,n-k))*2;

result.yhat = yhat;
result.ehat = ehat;
RSS  = b' * x' * y - n * mean(yhat)^2;
TSS  = y' * y - n * ybar^2;
result.R2   = (n-2)/(n-k) *  RSS/TSS;


