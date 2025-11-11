# server.R
server = function(input, output) {
  
  # default variables (to make the app start correctly)
  default.grid = setup.grid(10) # the grid of initial balls in the field
  default.fn = calculateFitnessFun(default.grid) # initial fitness function
  
  control = initECRControl(default.fn, n.objectives = 1, minimize = F)
  control = registerECROperator(control, "mutate", mutGauss, sdev = 2, lower = lower, upper = upper)
  control = registerECROperator(control, "selectForSurvival", selGreedy)
  
  default.pop = genReal(10, 2, lower, upper)
  default.fitness = evaluateFitness(control, default.pop)
  
  # define reactive values 
  init = reactiveValues(grid = default.grid, fn = default.fn, control = control)           # the grid can be changed by user
  plot.dat = reactiveValues(main = NULL, layer1 = NULL, layer2 = NULL, title = NULL)       # data for plots will be changed
  ea = reactiveValues(population = default.pop, population.fitness = default.fitness,      # reactive values for EA steps
                      variation = NULL, variation.fitness = NULL) 
  
  # observe changements of the grid  
  observe({
    n = input$nballs
    init$grid = setup.grid(n)
    init$fn = calculateFitnessFun(init$grid)
    
    # setup control 
    control = initECRControl(init$fn, n.objectives = 1, minimize = F)
    control = registerECROperator(control, "mutate", mutGauss, sdev = 2, lower = lower, upper = upper)
    init$control = registerECROperator(control, "selectForSurvival", selGreedy)
    control = init$control
    
    isolate({
      ea$population.fitness = evaluateFitness(init$control, ea$population)
    })
    
  })
  
  output$grid = renderPlot({
    plot.dat$main = plot.grid(init$grid) + plot.dat$title
    plot.dat$main + plot.dat$layer1 + plot.dat$layer2 
  })
  
  
  observeEvent(input$create.population, {
    ea$population = genReal(input$mu, 2, lower, upper)
    ea$population.fitness = evaluateFitness(init$control, ea$population)
    
    population = data.frame(t(matrix(unlist(ea$population), nrow = 2)), r = as.numeric(ea$population.fitness))
    
    plot.dat$layer1 = geom_circle(data = population, aes(x0 = X1, y0 = X2, r = r), fill = "royalblue2", colour = "royalblue2", alpha = 0.4)
  })
  
  observeEvent(input$variation, {
    # sample lambda individuals at random
    idx = sample(1:input$mu, input$lambda)
    # generate offspring by mutation and evaluate their fitness
    ea$variation = mutate(init$control, ea$population[idx], p.mut = 1)
    ea$variation.fitness = evaluateFitness(init$control, ea$variation)
    variation = data.frame(t(matrix(unlist(ea$variation), nrow = 2)), r = as.numeric(ea$variation.fitness))
    
    plot.dat$layer2 = geom_circle(data = variation, aes(x0 = X1, y0 = X2, r = r), fill = "violet", colour = "violet", alpha = 0.4)
    plot.dat$title = ggtitle(label = "Gauss-Mutation", subtitle = paste("Es wurden ", expression(lambda), " = ", input$lambda, "Nachkommen erzeugt"))
    
  })
  
  
  observeEvent(input$selection, {
    sel = replaceMuPlusLambda(init$control, ea$population, ea$variation, ea$population.fitness, ea$variation.fitness)
    ea$population = sel$population
    ea$population.fitness = sel$fitness
    
    population = data.frame(t(matrix(unlist(ea$population), nrow = 2)), r = as.numeric(ea$population.fitness))
    plot.dat$layer2 = NULL
    
    plot.dat$layer1 = geom_circle(data = population, aes(x0 = X1, y0 = X2, r = r), fill = "royalblue2", colour = "royalblue2", alpha = 0.4)
  })
  
  observe({
    print(input$slider)
    
    isolate({
      idx = sample(1:input$mu, input$lambda)
      
      ea$variation = mutate(init$control, ea$population[idx], p.mut = 1)
      ea$variation.fitness = evaluateFitness(init$control, ea$variation)
      variation = data.frame(t(matrix(unlist(ea$variation), nrow = 2)), r = as.numeric(ea$variation.fitness))
      
      
      sel = replaceMuPlusLambda(init$control, ea$population, ea$variation, ea$population.fitness, ea$variation.fitness)
      
      ea$population = sel$population
      ea$population.fitness = sel$fitness
      variation = NULL
      variation.fitness = NULL
      population = data.frame(t(matrix(unlist(ea$population), nrow = 2)), r = as.numeric(ea$population.fitness))
      
      plot.dat$layer1 = geom_circle(data = population, aes(x0 = X1, y0 = X2, r = r), fill = "royalblue2", colour = "royalblue2", alpha = 0.4)
      
      
      
    })
  })
  
  output$slider_to_anim <- renderUI({
    sliderInput("slider", 
      "Animation:", 
      min = 1, 
      max = 1000, 
      value = 1, 
      animate = animationOptions(interval = input$speed), width = '12cm')
  })
  
  output$speed_value <- renderUI({
    radioButtons("speed", label = "Speed", choices = c("x1" = 1000, "x3" = 500, "x10" = 100), selected = 1000, inline = T)
  })
  
}
