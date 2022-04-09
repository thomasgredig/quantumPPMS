.subVSMdata <- function(obj, m1) {
  nObj = obj

  nObj@time = nObj@time[m1]
  nObj@T = nObj@T[m1]
  nObj@H = nObj@H[m1]
  nObj@M = nObj@M [m1]
  nObj@Merr = nObj@Merr[m1]
  nObj@Temp = droplevels(nObj@Temp[m1])
  nObj@dir = nObj@dir[m1]
  nObj@loop = nObj@loop[m1]
  nObj@Mcorr = nObj@Mcorr[m1]
  nObj@type = nObj@type[m1]

  nObj
}
