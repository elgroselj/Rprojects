# dismis limbos?
# mutation probability?
# penalties?
# fix to NA dir after finish ?
# crossover is bad 101010 NA NA 101001

library(GA)

CROMO_LEN = NA
MAZE_DIM = NA
POP_SIZE = NA
MAX_ITER = NA
RUN = NA
GRID = NA


str_to_grid <- function(maze)
{
  # convert maze that is a vector of strings (that represent rows of maze) into grid of characters
  N <- length(maze)
  grid<-matrix(NA, nrow=N, ncol=N)
  
  for (i in 1:N)
  {
    grid[i,] <- strsplit(maze[i], "")[[1]] # change whole i-th row of grid to i-th row of maze splitted into characters
  }
  grid
}

init <- function(i) {
  # i is index of the maze in mazes
  # function that sets all the global variables according to the i-th maze
  mazes = c()
  maze_dim = c()
  
  mazes[[1]] = str_to_grid(c("##E##",
                             "#...#",
                             "#...#",
                             "#S..#",
                             "#####"))
  
  mazes[[2]] = str_to_grid(c("#####E#",
                             "#.#.#.#",
                             "#.....#",
                             "##.####",
                             "#....S#",
                             "#.#..##",
                             "#######"))
  mazes[[3]] = str_to_grid(c("##########",
                             "#S.......#",
                             "########.#",
                             "#........#",
                             "#.########",
                             "#........#",
                             "########.#",
                             "#........#",
                             "#.########",
                             "#E########"))
  
  mazes[[4]] = str_to_grid(c("#####E####",
                             "#...#....#",
                             "#.######.#",
                             "#.#......#",
                             "#.###.####",
                             "#....S...#",
                             "#####.##.#",
                             "#..##.####",
                             "##.......#",
                             "##########"))
  
  mazes[[5]] = str_to_grid(c("###E###########",
                             "#........###..#",
                             "########.....##",
                             "#.....##.###.##",
                             "#.###.....##..#",
                             "#.....###...###",
                             "####.####.#.###",
                             "##......#.....#",
                             "#######.#.###.#",
                             "#.........#####",
                             "#.####.S#....##",
                             "##....#.#######",
                             "#..##.........#",
                             "##...###.##.###",
                             "###############"))
  
  mazes[[6]] = str_to_grid(c("##################",
                             "##...#####...#####",
                             "##.#.##....#.....#",
                             "##.S....##...###.#",
                             "###.####.#.#.....#",
                             "#........#.##.####",
                             "##.#######......##",
                             "##......##########",
                             "#####.####....##.#",
                             "##.........##..#.#",
                             "#######.#####.##.#",
                             "####.##.##.....#.#",
                             "####.##.########.#",
                             "#................#",
                             "#..###############",
                             "##.#.##.#.##.##.##",
                             "##...............#",
                             "##########E#######"))
  
  mazes[[7]] = str_to_grid(c("####################",
                             "#..................#",
                             "#.##############.###",
                             "#.########.......###",
                             "#.#........######..#",
                             "#.##.##.##........##",
                             "#.#...#.##.######.##",
                             "#.###.#.##.....##.##",
                             "#.###..##########.##",
                             "#.####.###.........#",
                             "#.#....#...####.####",
                             "#.#.####.####.....##",
                             "#.#......#......####",
                             "#.###.####.#####...#",
                             "#.#......#.....#.###",
                             "#.######...#####.#.#",
                             "#.#.....##S#...#.#.#",
                             "#.#########..#.....#",
                             "#..........###.##.##",
                             "##########E#########"))
  
  for (m in 1:length(mazes))
  {
    maze_dim[m] = ncol(mazes[[m]])
  }
  
  cromo_len = maze_dim^2*2 # 5 => 25 * 2
  
  CROMO_LEN <<- cromo_len[i]
  MAZE_DIM <<- maze_dim[i]
  POP_SIZE <<- 500
  MAX_ITER <<- 200
  RUN <<- 25
  GRID <<- mazes[[i]]
}



encode_dir <- function(str)
{
  # str="LR"
  splited <- strsplit(str, "")[[1]]
  
  r <- c()
  di <- list(L=c(0,0), R=c(0,1), U=c(1,0), D=c(1,1))
  
  for (c in splited)
  {
    r <- c(r,di[c][[1]])
  }
  
  r_ = matrix(NA, nrow = 1, ncol = CROMO_LEN)
  r_[1:length(r)] = r
  r_
  return(r_)
}

map_code <- function(code)
{
  #code = encode_dir("LLLR")
  N <- MAZE_DIM
  map <- matrix(0, nrow=N, ncol=N)
  
  #get x and y position of start ()
  ind <- which(GRID=="S")
  v_S = ind%%N #row
  h_S = ind%/%N + 1 #column
  
  ind <- which(GRID=="E")
  v_E = ind%%N #row
  h_E = ind%/%N + 1 #column
  
  v = v_S
  h = h_S
  
  di_h <- c(-1,1,0,0)  #L R U D
  di_v <- c(0,0,-1,1)
  
  st <- 1 #number of steps, start is step 1
  map[v_S,h_S] <- st
  
  i = 1
  while (2*i <= length(code) & !is.na(code[2*i]))
  {
    # c = bit code to decimal (0 = Left, 1 = Right, 2 = Up, 3 = Down)
    c = code[2*i-1] * 2 + code[2*i] + 1 # 0*2+0 +1=> 0 +1 # +1 because index starts with 1
    v = v + di_v[c] #current row
    h = h + di_h[c] #current column
    
    if (!(h %in% 1:ncol(GRID) & v %in% 1:nrow(GRID))) #if path goes out of the map 
    {
      return (FALSE)
    }
    if (TRUE)#map[v,h]==0)
    {
      st <- st + 1
      map[v,h] = st
    }
    #else
    #{
    
    #st <- map[v,h] # do not count limbos
    #}
    
    if (v == v_E & h == h_E)
    {
      break
    }
    
    i = i +1
  }
  
  return (map)
  
}

code_len <- function(code)
{
  NAindex <- which(is.na(code))
  if (length(NAindex) == 0){
    return(CROMO_LEN)
  }
  
  firstNA <- min(NAindex)
  return(firstNA-1)
  
}

check_validity <- function(code)
{
  map <- map_code(code)
  if (is.null(ncol(map)))
  {
    return(FALSE)
  }
  return(length(which(GRID =="#" & map != 0)) == 0)
}

check_ends <- function(code)
{
  map <- map_code(code)
  return(map[GRID=="E"] > 0)
}



myFitness <- function(param)
{
  
  code <- param
  
  ####
  #code = encode_dir("LR", 5)
  ###
  
  map <- map_code(code)
  f <- 0
  #f <- -30*length(map[GRID=="#" & map!=0]) # stevilo korakov v steni
  
  
  #f <- f + MAZE_DIM*code_len(code) # da bi bla daljsa navodila
  
  if (is.na(code[1])){
    f <- f - MAZE_DIM^4
  }
  
  val_at_end <- map[which(GRID=="E")]
  f <- f - (val_at_end - length(map[map>0])) # num sekanj same s sabo
  if (is.na(val_at_end) | val_at_end == 0)
  {
    pen_no_finish <- MAZE_DIM^3
    f <- f - pen_no_finish # pen for not finishing ## to think about do we get such cases # mutation
    
  }
  else
  {
    f <- f - val_at_end # length of the whole path
  }
  f
  return(f)
}


myCrossover <- function(object, parents_ind)
{
  
  # you have parents
  # map/ narises pot z map_code
  # v katerih poljih sta oba neničelna
  # naj to ne bo začetek ali konec
  # naj to ne bo zadnji korak od enega od parentov (naj ne bo polje z vrednostjo enako dolžini korakov = dolžini nicel in enic/2 )
  # common so skupne točke zapisani v obliki indexov v 1D
  # izmenjaš rep
  # odreži na dolžino kromosoma
  
  fitness <- object@fitness[parents_ind]
  parents <- object@population[parents_ind,,drop = FALSE]
  
  n <- ncol(parents) #chromosome length
  children <- matrix(NA, nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  
  parent1_code = parents[1,]
  parent2_code = parents[2,]
  map1 = map_code(parent1_code)
  map2 = map_code(parent2_code)
  
  # common and not trivial
  common = which(map1!=0 & map2!=0 & GRID!="S" & GRID!="E")
  if (length(common) == 0)
  {
    children = parents
    #children[1,] <- normalize(children[1,])
    #children[2,] <- normalize(children[2,])
    fitnessChildren <- fitness
  }
  else
  {
    crossOverPoint <- common[sample(1:length(common), size = 1)]
    
    crossOverPoint_1 = map1[crossOverPoint]*2-2 #v katerem bitu v navodilih je točka sekanja
    crossOverPoint_2 = map2[crossOverPoint]*2-2
    
    # naj crossover point ne bo zadnji korak
    if (crossOverPoint_1 >= code_len(parent1_code)-2 | crossOverPoint_2 >= code_len(parent2_code)-2){
      print("-------------")
      children = parents
      fitnessChildren = fitness
    }else{
      children[1,] <- c(parents[1,1:(crossOverPoint_1)], parents[2,(crossOverPoint_2+1):n])[1:n] #oba otroka nastavi na dolžino n
      children[2,] <- c(parents[2,1:(crossOverPoint_2)], parents[1,(crossOverPoint_1+1):n])[1:n]
    }
    
  }
  if(!check_validity(children[1,]) | !check_validity(children[2,])){
    print(check_validity(children[1,]))
    print(check_validity(children[2,]))
    print("not valid crossover")
    print("parents:")
    print(crossOverPoint_1)
    print(crossOverPoint_2)
    print(result(parents[1,]))
    print(result(parents[2,]))
    print(parents[1,])
    print(parents[2,])
    print(parents[1,1:(crossOverPoint_1)])
    print(parents[2,1:(crossOverPoint_2)])
    print(children[1,])
    print(children[2,])
    children = parents
    fitnessChildren = fitness
  }
  out <- list(children = children, fitness = fitnessChildren)
  return(out)
}

myMutation <- function(object, parent)
{
  # select the parent from population
  mutate <- parent <- as.vector(object@population[parent,])
  # sample a random element that should be changed
  j <- sample(1:code_len(mutate), size=1)
  
  mutate[j] = (mutate[j]+1)%%2
  
  # ensure validity
  if (!check_validity(mutate))
  {
    return(parent)
  }
  
  return(mutate)
}

# myMutation_try <- function(object, parent)
# {
#   mutate <- parent <- as.vector(object@population[parent,])
#   
#   mutate = c()
#   mutate <-parent<- encode_dir("RDLLD")
#   encode_dir("D")
#   mutate
#   
#   j <- sample(1:(code_len(mutate)/2), size=2, replace=FALSE)
#   j<-c(2,4)
#   j = j*2-1
#   j = c(min(j),max(j))
#   cumu = c()
#   for (i in seq(from= j[1], to= j[2], step= 2))
#   {
#     i=j[1]
#       
#     gene = parent[i]*2+parent[i+1]+1
#     op = c(2,1,4,3)
#     gene_ = op[gene]-1
#     cumu = c(gene_%/%2,gene_%%2,cumu)
#   }
#   mutate[j[1]:(j[2]+1)]= cumu
#   
#   map_code(parent)
#   map_code(mutate)
#   # ensure validity
#   if (!check_validity(mutate))
#   {
#     return(parent)
#   }
#   
#   return(mutate)
# }


myPopulation <- function(object)
{
  pop = matrix(NA, nrow = object@popSize, ncol = object@nBits)
  
  di_h <- c(-1,1,0,0)  #L R U D
  di_v <- c(0,0,-1,1)
  di_op <- c(2,1,4,3) # the opposite direction's index
  
  ind <- which(GRID=="S")
  pos = c(ind%%MAZE_DIM,ind%/%MAZE_DIM + 1)
  
  i=1
  while (i <= nrow(pop)) {
    pos = c(ind%%MAZE_DIM,ind%/%MAZE_DIM + 1)
    for (j in seq(from= 1,to=ncol(pop), by= 2))
    {
      dirs = sample(1:4, size=4, replace = FALSE)
      dir_ = NA
      for (dir in dirs){
        if (j>2){
          if (dir == di_op[pop[i,j-2]*2 +pop[i,j-1]+1])
          {
            next
          }
        }
        v = pos[1]+di_v[dir]
        h = pos[2]+di_h[dir]
        #print(v,h)
        if (v %in% 1:nrow(GRID) & h %in% 1:ncol(GRID))
        {
          if (GRID[v, h] != "#")
          {
            pos = c(pos[1]+di_v[dir], pos[2]+di_h[dir])
            dir_ = dir
            break
          }
        }
        
      }
      
      if (is.na(dir_)){
        break
      }
      else
      {
        pop[i,j] = (dir_-1)%/%2
        pop[i,j+1] = (dir_-1)%%2
      }
    }
    if (!is.na(pop[i,1]) & check_validity(pop[i,])){
      i = i + 1
    }
  }
  
  
  return(pop)
}

result <- function(sol) {
  #sol = c(1,  0,  1,  0,  0,  1,  1,  0,  0,   1,  NA,  NA)
  r=""
  names = c("L","R","U","D")
  map = map_code(sol)
  map
  sol_size = map[GRID=="E"]
  sol_size
  if (sol_size == 0){
    return(FALSE)
  }
  
  
  for (i in seq(from=1, to=(sol_size-1)*2, by=2))
  {
    gene = sol[i]*2+sol[i+1] +1
    gene
    r = paste(r, names[gene], sep = "", collapse = NULL)
  }
  return(r)
}

start <- function(i) {
  init(i)
  
  GA <- ga(type = "binary", fitness = myFitness, nBits = CROMO_LEN,
           popSize = POP_SIZE, crossover = myCrossover, mutation = myMutation, maxiter = MAX_ITER, run = RUN, population = myPopulation)
  print(GA@solution)
  print(GRID)
  for(j in 1:nrow(GA@solution))
  {
    print(map_code(GA@solution[j,]))
  }
  
  plot(GA)
  #print(check_validity(GA@solution[1,]))
  print(apply(GA@solution,1,check_ends))
  #print(GA@population)
  #print(GA@solution[1,][1])
  print(GA@solution[1,])
  print(result(GA@solution[1,]))
  
  
}




