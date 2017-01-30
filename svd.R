library(jpeg)

setwd("~/SVD")
img = readJPEG("tiger.jpg")

red = img[,,1]
green = img[,,2]
blue = img[,,3]

red.svd = svd(red)
green.svd = svd(green)
blue.svd = svd(blue)

svd_list = list(red.svd, green.svd, blue.svd)

# first approximation 3 singular values
values=3
tiger3 =sapply(svd_list, function(M) {
  compress = M$u[,1:values] %*% diag(M$d[1:values]) %*% t(M$v[,1:values])
}, simplify = 'array')

writeJPEG(tiger3, paste('tiger', '_svd_', values, '.jpg', sep=''))


# second approximation 10 singular values
values=10
tiger30 =sapply(svd_list, function(M) {
  compress = M$u[,1:values] %*% diag(M$d[1:values]) %*% t(M$v[,1:values])
}, simplify = 'array')

writeJPEG(tiger10, paste('tiger', '_svd_', values, '.jpg', sep=''))

# third approximation 100 singular values
values=100
tiger100 =sapply(svd_list, function(M) {
  compress = M$u[,1:values] %*% diag(M$d[1:values]) %*% t(M$v[,1:values])
}, simplify = 'array')

writeJPEG(tiger100, paste('tiger', '_svd_', values, '.jpg', sep=''))

# final approximation 800 singular values
values=800
tiger800 =sapply(svd_list, function(M) {
  compress = M$u[,1:values] %*% diag(M$d[1:values]) %*% t(M$v[,1:values])
}, simplify = 'array')

writeJPEG(tiger800, paste('tiger', '_svd_', values, '.jpg', sep=''))