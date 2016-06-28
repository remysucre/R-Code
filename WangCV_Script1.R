## author Clara Wang
## GOV 016 / MSS 017
## Professor Horiuchi
## Figure 1 for final project -- figure comparing popularity of a US President and their first name
## May 2015


## --------------------------------------< PREPARE WORKSPACE >--------------------------------------------

rm(list = ls())
setwd("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1")

##install.packages("dplyr")
library(dplyr)


## ----------------------------------< LOAD AND MERGE BIRTH DATA >----------------------------------------

## load the data sets, each data set is the number of babies born in the US with the corresponding name
##yob1880 <- read.csv("yob1880.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1880" = V3)
##yob1881 <- read.csv("yob1881.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1881" = V3)
##yob1882 <- read.csv("yob1882.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1882" = V3)
##yob1883 <- read.csv("yob1883.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1883" = V3)
##yob1884 <- read.csv("yob1884.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1884" = V3)
##yob1885 <- read.csv("yob1885.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1885" = V3)
##yob1886 <- read.csv("yob1886.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1886" = V3)
##yob1887 <- read.csv("yob1887.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1887" = V3)
##yob1888 <- read.csv("yob1888.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1888" = V3)
##yob1889 <- read.csv("yob1889.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1889" = V3)
##yob1890 <- read.csv("yob1890.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1890" = V3)
##yob1891 <- read.csv("yob1891.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1891" = V3)
##yob1892 <- read.csv("yob1892.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1892" = V3)
##yob1893 <- read.csv("yob1893.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1893" = V3)
##yob1894 <- read.csv("yob1894.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1894" = V3)
##yob1895 <- read.csv("yob1895.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1895" = V3)
##yob1896 <- read.csv("yob1896.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1896" = V3)
##yob1897 <- read.csv("yob1897.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1897" = V3)
##yob1898 <- read.csv("yob1898.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1898" = V3)
##yob1899 <- read.csv("yob1899.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1899" = V3)
##yob1900 <- read.csv("yob1900.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1900" = V3)
##yob1901 <- read.csv("yob1901.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1901" = V3)
##yob1902 <- read.csv("yob1902.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1902" = V3)
##yob1903 <- read.csv("yob1903.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1903" = V3)
##yob1904 <- read.csv("yob1904.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1904" = V3)
##yob1905 <- read.csv("yob1905.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1905" = V3)
##yob1906 <- read.csv("yob1906.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1906" = V3)
##yob1907 <- read.csv("yob1907.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1907" = V3)
##yob1908 <- read.csv("yob1908.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1908" = V3)
##yob1909 <- read.csv("yob1909.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1909" = V3)
##yob1910 <- read.csv("yob1910.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1910" = V3)
##yob1911 <- read.csv("yob1911.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1911" = V3)
##yob1912 <- read.csv("yob1912.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1912" = V3)
##yob1913 <- read.csv("yob1913.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1913" = V3)
##yob1914 <- read.csv("yob1914.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1914" = V3)
##yob1915 <- read.csv("yob1915.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1915" = V3)
##yob1916 <- read.csv("yob1916.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1916" = V3)
##yob1917 <- read.csv("yob1917.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1917" = V3)
##yob1918 <- read.csv("yob1918.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1918" = V3)
##yob1919 <- read.csv("yob1919.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1919" = V3)
##yob1920 <- read.csv("yob1920.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1920" = V3)
##yob1921 <- read.csv("yob1921.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1921" = V3)
##yob1922 <- read.csv("yob1922.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1922" = V3)
##yob1923 <- read.csv("yob1923.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1923" = V3)
##yob1924 <- read.csv("yob1924.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1924" = V3)
##yob1925 <- read.csv("yob1925.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1925" = V3)
##yob1926 <- read.csv("yob1926.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1926" = V3)
##yob1927 <- read.csv("yob1927.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1927" = V3)
##yob1928 <- read.csv("yob1928.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1928" = V3)
##yob1929 <- read.csv("yob1929.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1929" = V3)
##yob1930 <- read.csv("yob1930.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1930" = V3)
##yob1931 <- read.csv("yob1931.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1931" = V3)
##yob1932 <- read.csv("yob1932.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1932" = V3)
##yob1933 <- read.csv("yob1933.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1933" = V3)
##yob1934 <- read.csv("yob1934.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1934" = V3)
##yob1935 <- read.csv("yob1935.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1935" = V3)
##yob1936 <- read.csv("yob1936.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1936" = V3)
##yob1937 <- read.csv("yob1937.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1937" = V3)
##yob1938 <- read.csv("yob1938.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1938" = V3)
##yob1939 <- read.csv("yob1939.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1939" = V3)
##yob1940 <- read.csv("yob1940.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1940" = V3)
##yob1941 <- read.csv("yob1941.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1941" = V3)
##yob1942 <- read.csv("yob1942.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1942" = V3)
##yob1943 <- read.csv("yob1943.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1943" = V3)
##yob1944 <- read.csv("yob1944.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1944" = V3)
##yob1945 <- read.csv("yob1945.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1945" = V3)
##yob1946 <- read.csv("yob1946.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1946" = V3)
##yob1947 <- read.csv("yob1947.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1947" = V3)
##yob1948 <- read.csv("yob1948.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1948" = V3)
##yob1949 <- read.csv("yob1949.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1949" = V3)
##yob1950 <- read.csv("yob1950.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1950" = V3)
##yob1951 <- read.csv("yob1951.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1951" = V3)
##yob1952 <- read.csv("yob1952.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1952" = V3)
##yob1953 <- read.csv("yob1953.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1953" = V3)
##yob1954 <- read.csv("yob1954.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1954" = V3)
##yob1955 <- read.csv("yob1955.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1955" = V3)
##yob1956 <- read.csv("yob1956.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1956" = V3)
##yob1957 <- read.csv("yob1957.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1957" = V3)
##yob1958 <- read.csv("yob1958.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1958" = V3)
##yob1959 <- read.csv("yob1959.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1959" = V3)
##yob1960 <- read.csv("yob1960.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1960" = V3)
##yob1961 <- read.csv("yob1961.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1961" = V3)
##yob1962 <- read.csv("yob1962.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1962" = V3)
##yob1963 <- read.csv("yob1963.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1963" = V3)
##yob1964 <- read.csv("yob1964.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1964" = V3)
##yob1965 <- read.csv("yob1965.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1965" = V3)
##yob1966 <- read.csv("yob1966.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1966" = V3)
##yob1967 <- read.csv("yob1967.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1967" = V3)
##yob1968 <- read.csv("yob1968.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1968" = V3)
##yob1969 <- read.csv("yob1969.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1969" = V3)
##yob1970 <- read.csv("yob1970.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1970" = V3)
##yob1971 <- read.csv("yob1971.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1971" = V3)
##yob1972 <- read.csv("yob1972.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1972" = V3)
##yob1973 <- read.csv("yob1973.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1973" = V3)
##yob1974 <- read.csv("yob1974.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1974" = V3)
##yob1975 <- read.csv("yob1975.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1975" = V3)
##yob1976 <- read.csv("yob1976.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1976" = V3)
##yob1977 <- read.csv("yob1977.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1977" = V3)
##yob1978 <- read.csv("yob1978.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1978" = V3)
##yob1979 <- read.csv("yob1979.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1979" = V3)
##yob1980 <- read.csv("yob1980.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1980" = V3)
##yob1981 <- read.csv("yob1981.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1981" = V3)
##yob1982 <- read.csv("yob1982.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1982" = V3)
##yob1983 <- read.csv("yob1983.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1983" = V3)
##yob1984 <- read.csv("yob1984.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1984" = V3)
##yob1985 <- read.csv("yob1985.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1985" = V3)
##yob1986 <- read.csv("yob1986.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1986" = V3)
##yob1987 <- read.csv("yob1987.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1987" = V3)
##yob1988 <- read.csv("yob1988.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1988" = V3)
##yob1989 <- read.csv("yob1989.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1989" = V3)
##yob1990 <- read.csv("yob1990.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1990" = V3)
##yob1991 <- read.csv("yob1991.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1991" = V3)
##yob1992 <- read.csv("yob1992.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1992" = V3)
##yob1993 <- read.csv("yob1993.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1993" = V3)
##yob1994 <- read.csv("yob1994.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1994" = V3)
##yob1995 <- read.csv("yob1995.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1995" = V3)
##yob1996 <- read.csv("yob1996.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1996" = V3)
##yob1997 <- read.csv("yob1997.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1997" = V3)
##yob1998 <- read.csv("yob1998.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1998" = V3)
##yob1999 <- read.csv("yob1999.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "1999" = V3)
##yob2000 <- read.csv("yob2000.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2000" = V3)
##yob2001 <- read.csv("yob2001.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2001" = V3)
##yob2002 <- read.csv("yob2002.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2002" = V3)
##yob2003 <- read.csv("yob2003.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2003" = V3)
##yob2004 <- read.csv("yob2004.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2004" = V3)
##yob2005 <- read.csv("yob2005.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2005" = V3)
##yob2006 <- read.csv("yob2006.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2006" = V3)
##yob2007 <- read.csv("yob2007.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2007" = V3)
##yob2008 <- read.csv("yob2008.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2008" = V3)
##yob2009 <- read.csv("yob2009.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2009" = V3)
##yob2010 <- read.csv("yob2010.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2010" = V3)
##yob2011 <- read.csv("yob2011.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2011" = V3)
##yob2012 <- read.csv("yob2012.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2012" = V3)
##yob2013 <- read.csv("yob2013.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2013" = V3)
##yob2014 <- read.csv("yob2014.txt", stringsAsFactors = FALSE, header = FALSE) %>% select(name = V1, sex = V2, "2014" = V3)


##data1 <- full_join(yob1880, yob1881, by = c("name", "sex"))
##data2 <- full_join(yob1882, yob1883, by = c("name", "sex"))
##data3 <- full_join(yob1884, yob1885, by = c("name", "sex"))
##data4 <- full_join(yob1886, yob1887, by = c("name", "sex"))
##data5 <- full_join(yob1888, yob1889, by = c("name", "sex"))
##data6 <- full_join(yob1890, yob1891, by = c("name", "sex"))
##data7 <- full_join(yob1892, yob1893, by = c("name", "sex"))
##data8 <- full_join(yob1894, yob1895, by = c("name", "sex"))
##data9 <- full_join(yob1896, yob1897, by = c("name", "sex"))
##data10 <- full_join(yob1898, yob1899, by = c("name", "sex"))
##data11 <- full_join(yob1900, yob1901, by = c("name", "sex"))
##data12 <- full_join(yob1902, yob1903, by = c("name", "sex"))
##data13 <- full_join(yob1904, yob1905, by = c("name", "sex"))
##data14 <- full_join(yob1906, yob1907, by = c("name", "sex"))
##data15 <- full_join(yob1908, yob1909, by = c("name", "sex"))
##data16 <- full_join(yob1910, yob1911, by = c("name", "sex"))
##data17 <- full_join(yob1912, yob1913, by = c("name", "sex"))
##data18 <- full_join(yob1914, yob1915, by = c("name", "sex"))
##data19 <- full_join(yob1916, yob1917, by = c("name", "sex"))
##data20 <- full_join(yob1918, yob1919, by = c("name", "sex"))
##data21 <- full_join(yob1920, yob1921, by = c("name", "sex"))
##data22 <- full_join(yob1922, yob1923, by = c("name", "sex"))
##data23 <- full_join(yob1924, yob1925, by = c("name", "sex"))
##data24 <- full_join(yob1926, yob1927, by = c("name", "sex"))
##data25 <- full_join(yob1928, yob1929, by = c("name", "sex"))
##data26 <- full_join(yob1930, yob1931, by = c("name", "sex"))
##data27 <- full_join(yob1932, yob1933, by = c("name", "sex"))
##data28 <- full_join(yob1934, yob1935, by = c("name", "sex"))
##data29 <- full_join(yob1936, yob1937, by = c("name", "sex"))
##data30 <- full_join(yob1938, yob1939, by = c("name", "sex"))
##data31 <- full_join(yob1940, yob1941, by = c("name", "sex"))
##data32 <- full_join(yob1942, yob1943, by = c("name", "sex"))
##data33 <- full_join(yob1944, yob1945, by = c("name", "sex"))
##data34 <- full_join(yob1946, yob1947, by = c("name", "sex"))
##data35 <- full_join(yob1948, yob1949, by = c("name", "sex"))
##data36 <- full_join(yob1950, yob1951, by = c("name", "sex"))
##data37 <- full_join(yob1952, yob1953, by = c("name", "sex"))
##data38 <- full_join(yob1954, yob1955, by = c("name", "sex"))
##data39 <- full_join(yob1956, yob1957, by = c("name", "sex"))
##data40 <- full_join(yob1958, yob1959, by = c("name", "sex"))
##data41 <- full_join(yob1960, yob1961, by = c("name", "sex"))
##data42 <- full_join(yob1962, yob1963, by = c("name", "sex"))
##data43 <- full_join(yob1964, yob1965, by = c("name", "sex"))
##data44 <- full_join(yob1966, yob1967, by = c("name", "sex"))
##data45 <- full_join(yob1968, yob1969, by = c("name", "sex"))
##data46 <- full_join(yob1970, yob1971, by = c("name", "sex"))
##data47 <- full_join(yob1972, yob1973, by = c("name", "sex"))
##data48 <- full_join(yob1974, yob1975, by = c("name", "sex"))
##data49 <- full_join(yob1976, yob1977, by = c("name", "sex"))
##data50 <- full_join(yob1978, yob1979, by = c("name", "sex"))
##data51 <- full_join(yob1980, yob1981, by = c("name", "sex"))
##data52 <- full_join(yob1982, yob1983, by = c("name", "sex"))
##data53 <- full_join(yob1984, yob1985, by = c("name", "sex"))
##data54 <- full_join(yob1986, yob1987, by = c("name", "sex"))
##data55 <- full_join(yob1988, yob1989, by = c("name", "sex"))
##data56 <- full_join(yob1990, yob1991, by = c("name", "sex"))
##data57 <- full_join(yob1992, yob1993, by = c("name", "sex"))
##data58 <- full_join(yob1994, yob1995, by = c("name", "sex"))
##data59 <- full_join(yob1996, yob1997, by = c("name", "sex"))
##data60 <- full_join(yob1998, yob1999, by = c("name", "sex"))
##data61 <- full_join(yob2000, yob2001, by = c("name", "sex"))
##data62 <- full_join(yob2002, yob2003, by = c("name", "sex"))
##data63 <- full_join(yob2004, yob2005, by = c("name", "sex"))
##data64 <- full_join(yob2006, yob2007, by = c("name", "sex"))
##data65 <- full_join(yob2008, yob2009, by = c("name", "sex"))
##data66 <- full_join(yob2010, yob2011, by = c("name", "sex"))
##data67 <- full_join(yob2012, yob2013, by = c("name", "sex"))


##data.a <- full_join(data1, data2, by = c("name", "sex"))
##data.b <- full_join(data3, data4, by = c("name", "sex"))
##data.c <- full_join(data5, data6, by = c("name", "sex"))
##data.d <- full_join(data7, data8, by = c("name", "sex"))
##data.e <- full_join(data9, data10, by = c("name", "sex"))
##data.f <- full_join(data11, data12, by = c("name", "sex"))
##data.g <- full_join(data13, data14, by = c("name", "sex"))
##data.h <- full_join(data15, data16, by = c("name", "sex"))
##data.i <- full_join(data17, data18, by = c("name", "sex"))
##data.j <- full_join(data19, data20, by = c("name", "sex"))
##data.k <- full_join(data21, data22, by = c("name", "sex"))
##data.l <- full_join(data23, data24, by = c("name", "sex"))
##data.m <- full_join(data25, data26, by = c("name", "sex"))
##data.n <- full_join(data27, data28, by = c("name", "sex"))
##data.o <- full_join(data29, data30, by = c("name", "sex"))
##data.p <- full_join(data31, data32, by = c("name", "sex"))
##data.q <- full_join(data33, data34, by = c("name", "sex"))
##data.r <- full_join(data35, data36, by = c("name", "sex"))
##data.s <- full_join(data37, data38, by = c("name", "sex"))
##data.t <- full_join(data39, data40, by = c("name", "sex"))
##data.u <- full_join(data41, data42, by = c("name", "sex"))
##data.v <- full_join(data43, data44, by = c("name", "sex"))
##data.w <- full_join(data45, data46, by = c("name", "sex"))
##data.x <- full_join(data47, data48, by = c("name", "sex"))
##data.y <- full_join(data49, data50, by = c("name", "sex"))
##data.z <- full_join(data51, data52, by = c("name", "sex"))
##data.aa <- full_join(data53, data54, by = c("name", "sex"))
##data.bb <- full_join(data55, data56, by = c("name", "sex"))
##data.cc <- full_join(data57, data58, by = c("name", "sex"))
##data.dd <- full_join(data59, data60, by = c("name", "sex"))
##data.ee <- full_join(data61, data62, by = c("name", "sex"))
##data.ff <- full_join(data63, data64, by = c("name", "sex"))
##data.gg <- full_join(data65, data66, by = c("name", "sex"))
##data.hh <- full_join(data67, yob2014, by = c("name", "sex"))


##data.a1 <- full_join(data.a, data.b, by = c("name", "sex"))
##data.a2 <- full_join(data.c, data.d, by = c("name", "sex"))
##data.a3 <- full_join(data.e, data.f, by = c("name", "sex"))
##data.a4 <- full_join(data.g, data.h, by = c("name", "sex"))
##data.a5 <- full_join(data.i, data.j, by = c("name", "sex"))
##data.a6 <- full_join(data.k, data.l, by = c("name", "sex"))
##data.a7 <- full_join(data.m, data.n, by = c("name", "sex"))
##data.a8 <- full_join(data.o, data.p, by = c("name", "sex"))
##data.a9 <- full_join(data.q, data.r, by = c("name", "sex"))
##data.a10 <- full_join(data.s, data.t, by = c("name", "sex"))
##data.a11 <- full_join(data.u, data.v, by = c("name", "sex"))
##data.a12 <- full_join(data.w, data.x, by = c("name", "sex"))
##data.a13 <- full_join(data.y, data.z, by = c("name", "sex"))
##data.a14 <- full_join(data.aa, data.bb, by = c("name", "sex"))
##data.a15 <- full_join(data.cc, data.dd, by = c("name", "sex"))
##data.a16 <- full_join(data.ee, data.ff, by = c("name", "sex"))
##data.a17 <- full_join(data.gg, data.hh, by = c("name", "sex"))


##data.b1 <- full_join(data.a1, data.a2, by = c("name", "sex"))
##data.b2 <- full_join(data.a3, data.a4, by = c("name", "sex"))
##data.b3 <- full_join(data.a5, data.a6, by = c("name", "sex"))
##data.b4 <- full_join(data.a7, data.a8, by = c("name", "sex"))
##data.b5 <- full_join(data.a9, data.a10, by = c("name", "sex"))
##data.b6 <- full_join(data.a11, data.a12, by = c("name", "sex"))
##data.b7 <- full_join(data.a13, data.a14, by = c("name", "sex"))
##data.b8 <- full_join(data.a15, data.a16, by = c("name", "sex"))
##data.b8 <- full_join(data.b8, data.a17, by = c("name", "sex"))


##data.c1 <- full_join(data.b1, data.b2, by = c("name", "sex"))
##data.c2 <- full_join(data.b3, data.b4, by = c("name", "sex"))
##data.c3 <- full_join(data.b5, data.b6, by = c("name", "sex"))
##data.c4 <- full_join(data.b7, data.b8, by = c("name", "sex"))


##data.d1 <- full_join(data.c1, data.c2, by = c("name", "sex"))
##data.d2 <- full_join(data.c3, data.c4, by = c("name", "sex"))


##data.full <- full_join(data.d1, data.d2, by = c("name", "sex"))


##write.csv(data.full, file = "birth_data.csv", row.names = FALSE)  # save csv file of merged data


## ------------------------------------< PREPARE NEW WORKSPACE >------------------------------------------

rm(list = ls())

##install.packages("ggplot2", "dplyr", "reshape", "reshape2", "RColorBrewer", "ggthemes", "gridExtra", "extrafont")
library(ggplot2)
library(dplyr)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(extrafont)

## import fonts from extrafont package and load onto Windows device
#font_import()
#loadfonts(device = "win")


## ------------------------------------------< PREPARE DATA >--------------------------------------------

options(stringsAsFactors = FALSE)

## NAME DATA
birth.data <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/birth_data.csv",
                       check.names = FALSE) 

birth.data.totals <- birth.data %>%
  melt(id = c("name", "sex"), check.names = FALSE) %>%
  filter(sex == "M") %>%
  group_by(variable, sex) %>%
  summarise(total = sum(value, na.rm = TRUE))


## TRUMAN DATA
truman <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/truman.csv") %>%
  select(president, year = start_year, approving) %>%
  group_by(president, year) %>%
  summarise(observation = median(approving)) %>%
  mutate(group = c("President's Popularity"))

df.truman <- birth.data %>%
  filter(name == "Harry") %>%
  filter(sex == "M") %>%
  melt(id = c("name", "sex"), check.names = FALSE) 

df.truman <- full_join(birth.data.totals, df.truman, by = c("variable", "sex")) %>%
  mutate(observation = value / total * 100) %>%
  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1938 & year <= 1958) %>%
  mutate(group = "Popularity of Name")

df.truman <- bind_rows(df.truman, truman) %>%
  mutate(president = c("Harry \n (President Truman)")) %>%
  mutate(term = c(0)) %>%
  mutate(term = ifelse(year >= 1945 & year < 1952, 1, term)) %>%
  mutate(group = factor(group,
                        levels = c("President's Popularity", "Popularity of Name")))


## KENNEDY DATA
kennedy <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/kennedy.csv") %>%
  select(president, year = start_year, approving) %>%
  group_by(president, year) %>%
  summarise(observation = median(approving)) %>%
  mutate(group = c("President's Popularity"))

df.kennedy <- birth.data %>%
  filter(name == "John") %>%
  filter(sex == "M") %>%
  melt(id = c("name", "sex"), check.names = FALSE) 

df.kennedy <- full_join(birth.data.totals, df.kennedy, by = c("variable", "sex")) %>%
  mutate(observation = value / total * 100) %>%
  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1954 & year <= 1970) %>%
  mutate(group = "Popularity of Name")

df.kennedy <- bind_rows(df.kennedy, kennedy) %>%
  mutate(president = c("John \n (President Kennedy)")) %>%
  mutate(term = c(0)) %>%
  mutate(term = ifelse(year >= 1961 & year < 1963, 1, term)) %>%
  mutate(group = factor(group,
                        levels = c("President's Popularity", "Popularity of Name")))


## NIXON DATA
nixon <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/nixon.csv") %>%
  select(president, year = start_year, approving) %>%
  group_by(president, year) %>%
  summarise(observation = median(approving)) %>%
  mutate(group = c("President's Popularity"))

df.nixon <- birth.data %>%
  filter(name == "Richard") %>%
  filter(sex == "M") %>%
  melt(id = c("name", "sex"), check.names = FALSE)

df.nixon <- full_join(birth.data.totals, df.nixon, by = c("variable", "sex")) %>%
  mutate(observation = value / total * 100) %>%
  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1962 & year <= 1980) %>%
  mutate(group = "Popularity of Name")

df.nixon <- bind_rows(df.nixon, nixon) %>%
  mutate(president = c("Richard \n (President Nixon)")) %>%
  mutate(term = c(0)) %>%
  mutate(term = ifelse(year >= 1969 & year < 1974, 1, term)) %>%
  mutate(group = factor(group,
                        levels = c("President's Popularity", "Popularity of Name")))


## CLINTON DATA
clinton <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/clinton.csv") %>%
  select(president, year = start_year, approving) %>%
  group_by(president, year) %>%
  summarise(observation = median(as.numeric(approving))) %>%
  mutate(group = c("President's Popularity")) %>%
  filter(year != 2001)

df.clinton <- birth.data %>%
  filter(name == "William") %>%
  filter(sex == "M") %>%
  melt(id = c("name", "sex"), check.names = FALSE) 

df.clinton <- full_join(birth.data.totals, df.clinton, by = c("variable", "sex")) %>%
  mutate(observation = value / total * 100) %>%
  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1986 & year <= 2006) %>%
  mutate(group = "Popularity of Name")

df.clinton <- bind_rows(df.clinton, clinton) %>%
  mutate(president = c("William \n (President Clinton)")) %>%
  mutate(term = c(0)) %>%
  mutate(term = ifelse(year >= 1993 & year < 2000, 1, term)) %>%
  mutate(group = factor(group,
                        levels = c("President's Popularity", "Popularity of Name")))


## BUSH JR DATA
bush_jr <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/bush_jr.csv") %>%
  select(president, year = start_year, approving) %>%
  group_by(president, year) %>%
  summarise(observation = median(as.numeric(approving))) %>%
  mutate(group = c("President's Popularity")) %>%
  filter(year != 2009)

df.bush.two <- birth.data %>%
  filter(name == "George") %>%
  filter(sex == "M") %>%
  melt(id = c("name", "sex"), check.names = FALSE) 

df.bush.two <- full_join(birth.data.totals, df.bush.two, by = c("variable", "sex")) %>%
  mutate(observation = value / total * 100) %>%
  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 1994 & year <= 2014) %>%
  mutate(group = "Popularity of Name")

df.bush.two <- bind_rows(df.bush.two, bush_jr) %>%
  mutate(president = c("George \n (President G.W. Bush)")) %>%
  mutate(term = c(0)) %>%
  mutate(term = ifelse(year >= 2001 & year < 2008, 1, term)) %>%
  mutate(group = factor(group,
                        levels = c("President's Popularity", "Popularity of Name")))


## OBAMA DATA
obama <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/obama.csv") %>%
  select(president, year = start_year, approving) %>%
  group_by(president, year) %>%
  summarise(observation = median(approving)) %>%
  mutate(group = c("President's Popularity"))

df.obama <- birth.data %>%
  filter(name == "Barack") %>%
  filter(sex == "M") %>%
  melt(id = c("name", "sex"), check.names = FALSE) 

df.obama <- full_join(birth.data.totals, df.obama, by = c("variable", "sex")) %>%
  mutate(observation = value / total * 100) %>%
  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
  ungroup() %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= 2002 & year <= 2014) %>%
  mutate(group = "Popularity of Name")

df.obama <- bind_rows(df.obama, obama) %>%
  filter(year != 2015) %>%
  mutate(president = c("Barack \n (President Obama)")) %>%
  mutate(term = c(0)) %>%
  mutate(term = ifelse(year >= 2009 & year < 2014, 1, term)) %>%
  mutate(group = factor(group,
                        levels = c("President's Popularity", "Popularity of Name")))


## COMPILE DATA
#pres1 <- rbind(df.truman, df.kennedy)
#pres2 <- rbind(df.nixon, df.clinton)
#pres3 <- rbind(df.bush.two, df.obama)

#pres.a <- rbind(pres1, pres2)
#pres.b <- rbind(pres.a, pres3)

#presidents <- rbind(pres.a, pres.b) %>%
#  mutate(president = factor(president,
#                            levels = c("Harry \n (President Truman)", "John \n (President Kennedy)", 
#                                       "Richard \n (President Nixon)", "William \n (President Clinton)", 
#                                       "George \n (President G.W. Bush)", "Barack \n (President Obama)"))) %>%
#  mutate(group = factor(group,
#                        levels = c("President's Popularity", "Popularity of Name"))) %>%
  #filter(president == "Richard \n (President Nixon)") %>%
  #filter(president == "William \n (President Clinton)") %>%
  #filter(president == "John \n (President Kennedy)") %>%
  #filter(president == "Barack \n (President Obama)") %>%
  #filter(president == "George \n (President G.W. Bush)") %>%
  #mutate(observation = ifelse(group == "Popularity of Name", log(observation), observation)) 




## ----------------------------------------< CREATE FIGURE >----------------------------------------------

## TRUMAN FIGURE
trumantext1 <- data.frame(year = 1948, observation = 72, 
                         lab = "Text",
                         group = factor("President's Popularity", 
                                        levels = c("President's Popularity", "Popularity of Name")))
trumanarrow1 <- data.frame(event = "Atomic Bomb\nDropped", 
                          group = factor("President's Popularity", 
                                         levels = c("President's Popularity", "Popularity of Name")))
trumantext2 <- data.frame(year = 1948, observation = 0.37, 
                         lab = "Text",
                         group = factor("Popularity of Name", 
                                        levels = c("President's Popularity", "Popularity of Name")))
trumanarrow2 <- data.frame(event = "Atomic Bomb\nDropped", 
                          group = factor("Popularity of Name", 
                                         levels = c("President's Popularity", "Popularity of Name")))

trumanplot <- ggplot() +
  geom_line(data = df.truman, 
             aes(x = year, 
                 y = observation,
                 alpha = term),
            size = 1.2,
            color = "#1f77b4",  # blue from tableau palette
            na.rm = TRUE) + 
  scale_alpha(range = c(0.5, 1)) +
  facet_grid(group ~ ., scales = "free_y") +
  scale_x_continuous(breaks = c(1936, 1940 ,1945, 1948, 1952, 1956, 1960)) +
  theme_bw() +
  ggtitle("Harry\n(President Truman)") +
  xlab("(in office 1945-1952)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, family = "Calibri"),
        axis.text.x = element_text(size = 9, angle = 90, family = "Calibri"),
        axis.text.y = element_text(family = "Calibri"),
        axis.title.x = element_text(size = 9, face = "italic", family = "Calibri"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0, 0.1, 0), "cm")) +
  geom_text(data = trumantext1, 
            aes(x = year, y = observation), 
            label = "Atomic Bomb\nDropped",
            size = 3,
            family = "Calibri") +
  geom_segment(data = trumanarrow1, aes(x = 1947.8, y = 76, xend = 1945.5, yend = 82),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_text(data = trumantext2, 
            aes(x = year, y = observation), 
            label = "Atomic Bomb\nDropped",
            size = 3,
            family = "Calibri") +
  geom_segment(data = trumanarrow2, aes(x = 1947.8, y = 0.385, xend = 1945.5, yend = 0.41),
               arrow = arrow(length = unit(0.1, "cm")))


## KENNEDY FIGURE
kennedytext1 <- data.frame(year = 1961, 
                           observation = 4, 
                           lab = "Text",
                       group = factor("Popularity of Name", 
                                      levels = c("President's Popularity", "Popularity of Name")))
kennedyarrow1 <- data.frame(event = "JFK Assassinated", 
                            group = factor("Popularity of Name", 
                                           levels = c("President's Popularity", "Popularity of Name")))

kennedyplot <- ggplot() +
  geom_line(data = df.kennedy, 
            aes(x = year, 
                y = observation,
                alpha = term),
            size = 1.2,
            color = "#ff7f0e",  # orange from tableau palette
            na.rm = TRUE) + 
  scale_alpha(range = c(0.5, 1)) +
  facet_grid(group ~ ., scales = "free_y") +
  scale_x_continuous(breaks = c(1954, 1958, 1961, 1963, 1966, 1970)) +
  theme_bw() +
  ggtitle("John\n(President Kennedy)") +
  xlab("(in office 1961-1963)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, family = "Calibri"),
        axis.text.x = element_text(size = 9, angle = 90, family = "Calibri"),
        axis.text.y = element_text(family = "Calibri"),
        axis.title.x = element_text(size = 9, face = "italic", family = "Calibri"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0, 0.1, 0), "cm")) +
  geom_text(data = kennedytext1, 
            aes(x = year, y = observation), 
            label = "JFK Assassinated",
            size = 3,
            family = "Calibri") +
  geom_segment(data = kennedyarrow1, aes(x = 1961.5, y = 3.95, xend = 1962.8, yend = 3.90),
               arrow = arrow(length = unit(0.1, "cm")))


## NIXON FIGURE
nixontext1 <- data.frame(year = 1973, observation = 59, 
                         lab = "Text",
                         group = factor("President's Popularity", 
                                        levels = c("President's Popularity", "Popularity of Name")))
nixonarrow1 <- data.frame(event = "Watergate", 
                          group = factor("President's Popularity", 
                                         levels = c("President's Popularity", "Popularity of Name")))
nixontext2 <- data.frame(year = 1974, observation = 1.65, 
                         lab = "Text",
                         group = factor("Popularity of Name", 
                                        levels = c("President's Popularity", "Popularity of Name")))
nixonarrow2 <- data.frame(event = "Watergate", 
                          group = factor("Popularity of Name", 
                                         levels = c("President's Popularity", "Popularity of Name")))

nixonplot <- ggplot() +
  geom_line(data = df.nixon, 
            aes(x = year, 
                y = observation,
                alpha = term),
            size = 1.2,
            color = "#2ca02c",  # green from tableau palette
            na.rm = TRUE ) + 
  scale_alpha(range = c(0.5, 1)) +
  facet_grid(group ~ ., scales = "free_y") +
  scale_x_continuous(breaks = c(1960, 1964, 1969, 1974, 1978, 1982)) +
  theme_bw() +
  ggtitle("Richard\n(President Nixon)") +
  xlab("(in office 1969-1974)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, family = "Calibri"),
        axis.text.x = element_text(size = 9, angle = 90, family = "Calibri"),
        axis.text.y = element_text(family = "Calibri"),
        axis.title.x = element_text(size = 9, face = "italic", family = "Calibri"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0, 0.1, 0), "cm")) +
  geom_text(data = nixontext1, 
            aes(x = year, y = observation), 
            label = "Watergate",
            size = 3,
            family = "Calibri") +
  geom_segment(data = nixonarrow1, aes(x = 1972.5, y = 58, xend = 1972, yend = 56),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_text(data = nixontext2, 
            aes(x = year, y = observation), 
            label = "Watergate",
            size = 3,
            family = "Calibri") +
  geom_segment(data = nixonarrow2, aes(x = 1973.5, y = 1.6, xend = 1972, yend = 1.4),
               arrow = arrow(length = unit(0.1, "cm")))


## CLINTON FIGURE  
clintontext1 <- data.frame(year = 1996, observation = 60, 
                         lab = "Text",
                         group = factor("President's Popularity", 
                                        levels = c("President's Popularity", "Popularity of Name")))
clintonarrow1 <- data.frame(event = "Impeachment", 
                          group = factor("President's Popularity", 
                                         levels = c("President's Popularity", "Popularity of Name")))
clintontext2 <- data.frame(year = 1998, observation = 1.2, 
                         lab = "Text",
                         group = factor("Popularity of Name", 
                                        levels = c("President's Popularity", "Popularity of Name")))
clintonarrow2 <- data.frame(event = "Impeachment", 
                          group = factor("Popularity of Name", 
                                         levels = c("President's Popularity", "Popularity of Name")))

clintonplot <- ggplot() +
  geom_line(data = df.clinton, 
            aes(x = year, 
                y = observation,
                alpha = term),
            size = 1.2,
            color = "#d62728",  # red from tableau palette
            na.rm = TRUE) + 
  scale_alpha(range = c(0.5, 1)) +
  facet_grid(group ~ ., scales = "free_y") +
  scale_x_continuous(breaks = c(1984, 1988, 1993, 1996, 2000, 2004, 2008)) +
  theme_bw() +
  ggtitle("William\n(President Clinton)") +
  xlab("(in office 1993-2000)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, family = "Calibri"),
        axis.text.x = element_text(size = 9, angle = 90, family = "Calibri"),
        axis.text.y = element_text(family = "Calibri"),
        axis.title.x = element_text(size = 9, face = "italic", family = "Calibri"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0, 0.1, 0), "cm")) +
  geom_text(data = clintontext1, 
            aes(x = year, y = observation), 
            label = "Impeachment",
            size = 3,
            family = "Calibri") +
  geom_segment(data = clintonarrow1, aes(x = 1992.7, y = 60.5, xend = 1997.6, yend = 63),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_text(data = clintontext2, 
            aes(x = year, y = observation), 
            label = "Impeachment",
            size = 3,
            family = "Calibri") +
  geom_segment(data = clintonarrow2, aes(x = 1998, y = 1.18, xend = 1998, yend = 1.11),
               arrow = arrow(length = unit(0.1, "cm")))


## BUSH JR FIGURE
bushtext1 <- data.frame(year = 1996, observation = 60, 
                           lab = "Text",
                           group = factor("President's Popularity", 
                                          levels = c("President's Popularity", "Popularity of Name")))
busharrow1 <- data.frame(event = "9/11", 
                            group = factor("President's Popularity", 
                                           levels = c("President's Popularity", "Popularity of Name")))
bushtext2 <- data.frame(year = 2008, observation = 0.16, 
                           lab = "Text",
                           group = factor("Popularity of Name", 
                                          levels = c("President's Popularity", "Popularity of Name")))
busharrow2 <- data.frame(event = "9/11", 
                            group = factor("Popularity of Name", 
                                           levels = c("President's Popularity", "Popularity of Name")))

bushjrplot <- ggplot() +
  geom_line(data = df.bush.two, 
            aes(x = year, 
                y = observation,
                alpha = term),
            size = 1.2,
            color = "#9467bd",  # purple from tableau palette
            na.rm = TRUE) + 
  scale_alpha(range = c(0.5, 1)) +
  facet_grid(group ~ ., scales = "free_y") +
  scale_x_continuous(breaks = c(1992, 1996, 2001, 2004, 2008, 2012, 2016)) +
  theme_bw() +
  ggtitle("George\n(President Bush Jr.)") +
  xlab("(in office 2001-2008)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, family = "Calibri"),
        axis.text.x = element_text(size = 9, angle = 90, family = "Calibri"),
        axis.text.y = element_text(family = "Calibri"),
        axis.title.x = element_text(size = 9, face = "italic", family = "Calibri"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0, 0.1, 0), "cm")) +
  geom_text(data = bushtext1, 
            aes(x = year, y = observation), 
            label = "9/11",
            size = 3,
            family = "Calibri") +
  geom_segment(data = busharrow1, aes(x = 2000, y = 60, xend = 2000.6, yend = 58),
               arrow = arrow(length = unit(0.1, "cm"))) +
  geom_text(data = bushtext2, 
            aes(x = year, y = observation), 
            label = "9/11",
            size = 3,
            family = "Calibri") +
  geom_segment(data = busharrow2, aes(x = 2004, y = 0.16, xend = 2001.6, yend = 0.16),
               arrow = arrow(length = unit(0.1, "cm")))


## OBAMA FIGURE
obamatext1 <- data.frame(year = 2006, observation = 0.0015, 
                           lab = "Text",
                           group = factor("Popularity of Name", 
                                          levels = c("President's Popularity", "Popularity of Name")))
obamaarrow1 <- data.frame(event = "Announce\nCampaign", 
                            group = factor("Popularity of Name", 
                                           levels = c("President's Popularity", "Popularity of Name")))

obamaplot <- ggplot() +
  geom_line(data = df.obama, 
            aes(x = year, 
                y = observation,
                alpha = term),
            size = 1.2,
            color = "#8c564b",  # brown from tableau palette
            na.rm = TRUE) + 
  scale_alpha(range = c(0.5, 1)) +
  facet_grid(group ~ ., scales = "free_y") +
  scale_x_continuous(breaks = c(2000, 2004, 2009, 2012, 2016)) +
  theme_bw() +
  ggtitle("Barack\n(President Obama)") +
  xlab("(in office 2009-present)") +
  theme(legend.position = "none",
        plot.title = element_text(size = 10, family = "Calibri"),
        axis.text.x = element_text(size = 9, angle = 90, family = "Calibri"),
        axis.text.y = element_text(family = "Calibri"),
        axis.title.x = element_text(size = 9, face = "italic", family = "Calibri"),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.2, 0, 0.1, 0), "cm")) +
  geom_text(data = obamatext1, 
            aes(x = year, y = observation), 
            label = "Announce\nCampaign",
            size = 3,
            family = "Calibri") +
  geom_segment(data = obamaarrow1, aes(x = 2004.5, y = 0.0012, xend = 2006.8, yend = 0.0007),
               arrow = arrow(length = unit(0.1, "cm")))


## FIGURES DRAWN TOGETHER IN A ROW
grid.arrange(arrangeGrob(trumanplot, kennedyplot, nixonplot, clintonplot, bushjrplot, obamaplot, ncol = 6),
             main = textGrob("Does a President's Popularity Affect the Popularity of their Name?",
                             vjust = 1,
                             gp = gpar(fontsize = 20, fontfamily = "Calibri", fontface = "bold")),
             left = textGrob("Popularity of Name Among US Males                                 President's Popularity
      (% of babies born with name)                                        (Approval Rating %)", 
                             rot = 90, 
                             vjust = .8,
                             gp = gpar(fontsize = 11, fontfamily = "Calibri", fontface = "bold")),
             bottom = textGrob("Year", 
                               rot = 0, 
                               gp = gpar(fontsize = 11, fontfamily = "Calibri", fontface = "bold")),
                               heights = c(10, .5))


## ----------------------------------------< UNUSED DATA >----------------------------------------------

#johnson <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/johnson.csv") %>%
#  select(president, year = start_year, approving) %>%
#  group_by(president, year) %>%
#  summarise(approval = median(approving))

#ford <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/ford.csv") %>%
#  select(president, year = start_year, approving) %>%
#  group_by(president, year) %>%
#  summarise(approval = median(approving))

#carter <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/carter.csv") %>%
#  select(president, year = start_year, approving) %>%
#  group_by(president, year) %>%
#  summarise(approval = median(approving))

#reagan <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/reagan.csv") %>%
#  select(president, year = start_year, approving) %>%
#  group_by(president, year) %>%
#  summarise(approval = median(as.numeric(approving)))

#bush_sr <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/bush_sr.csv") %>%
#  select(president, year = start_year, approving) %>%
#  group_by(president, year) %>%
#  summarise(approval = median(approving))



#df.carter <- birth.data %>%
#  filter(name == "Jimmy") %>%
#  filter(sex == "M") %>%
#  melt(id = c("name", "sex"), check.names = FALSE) 

#df.carter <- full_join(birth.data.totals, df.carter, by = c("variable", "sex")) %>%
#  filter(variable == 1974 | variable == 1975 | variable == 1976 | variable == 1977 | variable == 1978 |
#           variable == 1979 | variable == 1980 | variable == 1981 | variable == 1982) %>%
#  mutate(percent = value / total) %>%
#  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
#  ungroup() %>%
#  mutate(year = as.numeric(as.character(year)))

#df.carter <- full_join(df.carter, carter, by = "year") %>%
#  mutate(president = c("carter"))


#df.reagan <- birth.data %>%
#  filter(name == "Ronald") %>%
#  filter(sex == "M") %>%
#  melt(id = c("name", "sex"), check.names = FALSE) 

#df.reagan <- full_join(birth.data.totals, df.reagan, by = c("variable", "sex")) %>%
#  filter(variable == 1978 | variable == 1979 | variable == 1980 | variable == 1981 | variable == 1982 |
#           variable == 1983 | variable == 1984 | variable == 1985 | variable == 1986 | variable == 1987 |
#           variable == 1988 | variable == 1989 | variable == 1990) %>%
#  mutate(percent = value / total) %>%
#  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
#  ungroup() %>%
#  mutate(year = as.numeric(as.character(year)))

#df.reagan <- full_join(df.reagan, reagan, by = "year") %>%
#  mutate(president = c("reagan"))


#df.bush.one <- birth.data %>%
#  filter(name == "George") %>%
#  filter(sex == "M") %>%
#  melt(id = c("name", "sex"), check.names = FALSE)

#df.bush.one <- full_join(birth.data.totals, df.bush.one, by = c("variable", "sex")) %>%
#  filter(variable == 1986 | variable == 1987 | variable == 1988 | variable == 1989 | variable == 1990 |
#           variable == 1991 | variable == 1992 | variable == 1993 | variable == 1994) %>%
#  mutate(percent = value / total) %>%
#  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
#  ungroup() %>%
#  mutate(year = as.numeric(as.character(year)))

#df.bush.one <- full_join(df.bush.one, bush_sr, by = "year") %>%
#  mutate(president = c("bush_sr"))


#df.johnson <- birth.data %>%
#  filter(name == "Lyndon") %>%
#  filter(sex == "M") %>%
#  melt(id = c("name", "sex"), check.names = FALSE) 

#df.johnson <- full_join(birth.data.totals, df.johnson, by = c("variable", "sex")) %>%
#  filter(variable == 1960 | variable == 1961 | variable == 1962 | variable == 1963 | variable == 1964 |
#           variable == 1965 | variable == 1966 | variable == 1967 | variable == 1968 | variable == 1969 |
#           variable == 1970) %>%
#  mutate(percent = value / total) %>%
#  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
#  ungroup() %>%
#  mutate(year = as.numeric(as.character(year)))

#df.johnson <- full_join(df.johnson, johnson, by = "year") %>%
#  mutate(president = c("johnson"))




## EISENHOWER DATA
#eisenhower <- read.csv("C:/Users/Clara/Desktop/f001c1f/15S/GOV 016/Data for Project/Project_Figure1/eisenhower.csv") %>%
#  select(president, year = start_year, approving) %>%
#  group_by(president, year) %>%
#  summarise(observation = median(as.numeric(approving))) %>%
#  mutate(group = c("President's Popularity"))

#df.eisenhower <- birth.data %>%
#  filter(name == "Dwight") %>%
#  filter(sex == "M") %>%
#  melt(id = c("name", "sex"), check.names = FALSE) 

#df.eisenhower <- full_join(birth.data.totals, df.eisenhower, by = c("variable", "sex")) %>%
#  mutate(observation = log(value / total * 100)) %>%
#  rename(c(total = "total_ppl", value = "num_ppl", variable = "year")) %>%
#  ungroup() %>%
#  mutate(year = as.numeric(as.character(year))) %>%
#  filter(year >= 1946 & year <= 1966) %>%
#  mutate(group = "Popularity of Name")

#df.eisenhower <- bind_rows(df.eisenhower, eisenhower) %>%
#  mutate(president = c("Dwight \n (President Eisenhower)")) %>%
#  mutate(term = c(0)) %>%
#  mutate(term = ifelse(year >= 1953 & year <= 1961, 1, term))

