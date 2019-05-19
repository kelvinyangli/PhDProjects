# the polytrees that satisfiy the MB adjacencies is denoted by MBPTs
# nMBPTs is the function that computes the number of MBPTs in a function of mb size
# this formula has been manually verified by drawing all dags in a mb with up to 6 variables in a mb
# nWithNoSp is a function to computes the number of mb dags with no spouses, i.e., there is no collider
# n is the total number of variables in mb
nWithNoSp = function(n) 2 ^ n

# nWithSp is a function to computes the number of mb dags with spouses, i.e., there exists colliders
# n is the total number of variables, m is the number of colliders, and k is the number of spouses
nWithSp = function(n, m, k) {

  if (n < 2) {# not enough nodes to form a collider

    count = 0

  } else {

    count = choose(n, k + 1) * (k + 1)

    if (m < 2) {

      count = count * nWithNoSp(n - k - 1)

    } else {

      sum_count = 0

      for (k_dash in 1:min((n - k - 2 * m + 2), k)) {
        # the maximum that k_dash can be is (n-k-1)-2(m-2)-1, where (n-k-1) is # remaining
        # vars after removing k spouses and 1 common child, 2(m-2) is the minimum # vars
        # needed for having m-2 v-structures, and -1 removes another child
        sub_count = nWithSp(n - k - 1, m - 1, k_dash)

        if (k_dash == k) sub_count = (1 / m) * sub_count

        sum_count = sum_count + sub_count

      } # end for k_dash

      count = count * sum_count

    } # end else

  } # end else

  return(count)

}

nMBPTs = function(n, debug = F) {

  sum_count = nWithNoSp(n)
  if (debug) {

    cat("-------------------------------\n")
    cat("|mb|=", n, "\n")
    cat("|spouses|=", 0, "=> |mbDags|=", sum_count, "\n")
    cat("-------------------------------\n")

  }

  for (m in 1:floor(n / 2)) {

    if (debug) cat("|colliders|=", m, "\n")

    for (k in 1:(n - 2 * m + 1)) {

      sub_count = nWithSp(n, m, k)
      sum_count = sum_count + sub_count
      if (debug) cat("|spouses|=", k, "=> |mbDags|=", sub_count, "\n")

    } # end for k

    if (debug) cat("-------------------------------\n")

  } # end for m

  return(sum_count)

}

# this is the number of DAGs on n labelled variables
# this fomula is from wikipedia
nDags = function(n) {

  count = 0

  if (n < 1) {

    count = 1

  } else {

    for (i in 1:n) {

      count = count + ((-1) ^ (i - 1)) * choose(n, i) * (2 ^ (i * (n - i))) * nDags(n - i)

    } # end for i

  }

  return(count)

}


# the polytrees that satisfiy the MB adjacencies is denoted by MBPTs
# nMBPTs is the function that computes the number of MBPTs in a function of mb size
# this formula has been manually verified by drawing all dags in a mb with up to 6 variables in a mb
# nWithNoSp is a function to computes the number of mb dags with no spouses, i.e., there is no collider
# n is the total number of variables in mb
# p is the max fan-in
nWithNoSp_fpt = function(n, p) {

  count = 0

  for (i in 0:p) count = count + choose(n, i)

  return(count)

}

# nWithSp is a function to computes the number of mb dags with spouses, i.e., there exists colliders
# n is the total number of variables, m is the number of colliders, and k is the number of spouses
nWithSp_fpt = function(n, p, m, k) {

  if (n < 2) {# not enough nodes to form a collider

    count = 0

  } else {

    count = choose(n, k + 1) * (k + 1)

    if (m < 2) {

      count = count * nWithNoSp_fpt(n - k - 1, p)

    } else {

      sum_count = 0

      for (k_dash in 1:min((n - k - 2 * m + 2), k)) {
        # the maximum that k_dash can be is (n-k-1)-2(m-2)-1, where (n-k-1) is # remaining
        # vars after removing k spouses and 1 common child, 2(m-2) is the minimum # vars
        # needed for having m-2 v-structures, and -1 removes another child
        sub_count = nWithSp_fpt(n - k - 1, p, m - 1, k_dash)

        if (k_dash == k) sub_count = (1 / m) * sub_count

        sum_count = sum_count + sub_count

      } # end for k_dash

      count = count * sum_count

    } # end else

  } # end else

  return(count)

}

nMBPTs_fpt = function(n, p, debug = F) {

  sum_count = nWithNoSp_fpt(n, p)
  if (debug) {

    cat("-------------------------------\n")
    cat("|mb|=", n, "max fan-in", p, "\n")
    cat("|spouses|=", 0, "=> |mbDags|=", sum_count, "\n")
    cat("-------------------------------\n")

  }

  for (m in 1:floor(n / 2)) {

    if (debug) cat("|colliders|=", m, "\n")

    bound = min(n - 2 * m + 1, p - 1)
    for (k in 1:bound) {

      sub_count = nWithSp_fpt(n, p, m, k)
      sum_count = sum_count + sub_count
      if (debug) cat("|spouses|=", k, "=> |mbDags|=", sub_count, "\n")

    } # end for k

    if (debug) cat("-------------------------------\n")

  } # end for m

  return(sum_count)

}

n = 1:10
x = sapply(n, nMBPTs_fpt, p = 2)
y = sapply(n, nMBPTs_fpt, p = 1)
z = sapply(n, nMBPTs)
plot(n,z,type = "l",col="red")
lines(n,x,col="blue")
lines(n,y,col="green")


