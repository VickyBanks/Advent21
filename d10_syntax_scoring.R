
library(tidyverse)
library(readr)
library(stringi)
library(textclean)

x<- data.frame(nav = 
  c('[({(<(())[]>[[{[]{<()<>>',
      '[(()[<>])]({[<{<<[]>>(',
      '{([(<{}[<>[]}>{[]{[(<()>',
      '(((({<>}<{<{<>}{[]{[]{}',
      '[[<[([]))<([[{}[[()]]]',
      '[{[{({}]{}}([{[{{{}}([]',
      '{<[[]]>}<{[{[{[]{()[[[]',
      '[<(<(<(<{}))><([]([]()',
      '<{([([[(<>()){}]>(<<{{',
      '<{([{{}}[<[[[<>{}]]]>[]]')
)

for(row in 1:nrow(x)){
if(str_count(x$nav[row],pattern= '\\(' ) == str_count(x$nav[row],pattern= '\\)')
   & str_count(x$nav[row],pattern= '\\[' ) == str_count(x$nav[row],pattern= '\\]')
   & str_count(x$nav[row],pattern= '\\{' ) == str_count(x$nav[row],pattern= '\\}')
   & str_count(x$nav[row],pattern= '\\<' ) == str_count(x$nav[row],pattern= '\\>')
   ){
  print(paste0("row ",row," - match" ))
} else {print(paste0("row ",row," - no match" ))}
}



