
%% Load data
dd = csvread('25_Portfolios_5x5.CSV',1,0);
prlDates = yyyymm2date(dd(:,1));
prlRets   = dd(:,2:end)/100;

dd = csvread('F-F_Research_Data_Factors.csv',1,0);
factorDates = yyyymm2date(dd(:,1));
factorRets  = dd(:,2:end)/100;

%% Synchronize dates
[dates l r] = intersect(prlDates, factorDates);
prlRets = prlRets(l,:);
factorRets = factorRets(r,:);

% Plot to check that it looks resonable.
plot(dates,log(cumprod([prlRets/100+1 factorRets/100+1]))); datetick;

%% Part a
% 
% Estimate R_i - r_f = a + b r_m
%
%
rm = factorRets(:,1);
rf = factorRets(:,4);

excessRet = prlRets - repmat(rf,1,25);

avgRets = mean(excessRet)';
alphas  = zeros(25,1);
betas   = zeros(25,1);
alphasT = zeros(25,1);

x = [ones(size(rm)) rm];
for pp = 1:25
    res = ols(excessRet(:,pp),x);
    alphas(pp) = res.b(1);
    betas(pp) = res.b(2);
    alphasT(pp) = res.t(1);
end

% Stack them to look at alphas
% Down columns increases book to market.
% Across rows increases size
alp = reshape(alphas,5,5);

%% Part c
% Regress avg return on beta.

res = ols(avgRets,[ones(25,1),betas]);

%% Part d 
%
% Get CAPM prediction

mktAvg = mean(rm);
capmPrediction = mktAvg * betas;

scatter(avgRets,capmPrediction); xlim([0,.016]); ylim([0,.016]); xlabel('actual'); ylabel('prediction');

%% Now use 3 factor model

betas = zeros(25,4);
betasT = zeros(25,4);
x = [ones(length(rm),1) factorRets(:,1:3)];

for pp = 1:25
    res = ols(excessRet(:,pp),x);
    betas(pp,:) = res.b';
    betasT(pp,:) = res.t';
end

subplot(4,1,1); bar(betas(:,1)); title('\alpha');
subplot(4,1,2); bar(betas(:,2)); title('\beta_m');
subplot(4,1,3); bar(betas(:,3)); title('\beta_{smb}');
subplot(4,1,4); bar(betas(:,4)); title('\beta_{hml}');

% Stack them. Down rows is changing value; across col is changing size.
% Upper left is small, low book to market.
% Lower left is small, high book to market.
mktLoading = reshape(betas(:,2),5,5);
smbLoading = reshape(betas(:,3),5,5);
hmlLoading = reshape(betas(:,4),5,5);

factorAvgs = mean(factorRets(:,1:3))';
ff3Prediction = betas(:,2:end) * factorAvgs;

hold on
scatter(avgRets,capmPrediction,'MarkerFaceColor','b');
scatter(avgRets,ff3Prediction,'MarkerFaceColor','r');
plot(linspace(0,.016,5),linspace(0,.016,5),'color','g');
ylim([0,.016]); xlabel('actual'); ylabel('prediction');
legend({'CAPM','FF3'},'location','best');
hold off

,avgRets,ff3Prediction); xlim([0,1.6]); ylim([0,1.6]);
scatter(avgRets,capmPrediction); xlim([0,1.6]); ylim([0,1.6]); xlabel('actual'); ylabel('prediction');

%% PCA

sampleCov = cov(excessRet);
[V D] = eigs(sampleCov,eye(25),25);
eigenValues = diag(D);


% Plot them
bar(sqrt(eigenValues))

% Get loadings and arrange them
loadings = V(:,end-3:end);

% Reshape them---down rows is value; across cols is size
PC1 = reshape(loadings(:,end),5,5);
PC2 = reshape(loadings(:,end-1),5,5);
PC3 = reshape(loadings(:,end-2),5,5);
PC4 = reshape(loadings(:,end-3),5,5);


