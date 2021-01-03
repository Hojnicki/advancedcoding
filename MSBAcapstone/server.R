# server for the app
server <- function(input, output) {
    
    output$tester <- renderPlot({
        t <- df %>% 
            group_by(Tester_ID, Club) %>% 
            count() %>% 
            ggplot(aes(x=Tester_ID, y=n)) +
            geom_point(aes(color = factor(Club))) +
            #geom_segment(aes(x=Tester_ID, xend =Tester_ID, y = 0 , yend = n), color = 'black') +
            theme_minimal() + 
            coord_flip() +
            ylab("Count")
        t
    })
    
    output$group <- renderPlot({
        t <- df %>% 
            group_by(Group) %>% 
            count() %>% 
            ggplot(aes(x=Group, y=n)) +
            geom_point(color = 'red', size =4) +
            geom_segment(aes(x=Group, xend =Group, y = 0 , yend = n), color = 'black') +
            coord_flip() +
            theme_minimal()+
            ylab("Count")
            
        t
    })
    
    output$fullClub <- renderPlot({
        t <- df %>% 
            group_by(Full_Club_Name) %>% 
            count() %>% 
            ggplot(aes(x=Full_Club_Name, y=n)) +
            geom_point(color = 'blue', size =4) +
            geom_segment(aes(x=Full_Club_Name, xend =Full_Club_Name, y = 0 , yend = n), color = 'black') +
            theme_minimal() + 
            coord_flip() +
            ylab("Count")
        t
    })
    
    output$testDate <- renderPlot({
        t <- df %>% 
            group_by(Test_Date) %>% 
            count() %>% 
            ggplot(aes(x=Test_Date, y=n)) +
            geom_point(color = 'purple', size =4) +
            geom_segment(aes(x=Test_Date, xend =Test_Date, y = 0 , yend = n), color = 'black') +
            theme_minimal() + 
            coord_flip() +
            ylab("Count")
        t
    })
    
    output$sum <- renderPrint(summary(df))
    
    output$corr1 <- renderPlot({
        corrplot(res, type = "upper", order = "hclust", 
                 tl.col = "black", tl.srt = 45)
    }
    )
    
    output$corr2 <- renderPlot({
        col<- colorRampPalette(c("blue", "white", "red"))(20)
        heatmap(x = res, col = col, symm = TRUE)
    })
    
    output$relation <- renderPlot({
        ggplot(df, aes_string(x = input$var1, y = input$var2, fill = as.factor(df$OUTLIER_Present. ),  group = as.factor(df$OUTLIER_Present. ))) +
            geom_point(aes(col = as.factor(df$OUTLIER_Present. )),size = .5) +
            labs(color = 'Outliers Present') +
            scale_color_manual(labels = c('No','Yes'),
                                values = c('black','red')) +
            geom_smooth(method = lm, se=TRUE) +
            theme_minimal()+ 
            ggtitle("Plot of all data points") 
    })
    
    output$relation2 <- renderPlot({
        df %>% filter(OUTLIER_Present. == 0) %>%
            ggplot(aes_string(x = input$var1, y = input$var2)) +
            geom_point(size = .5) +
            geom_smooth(method = lm, se=TRUE) +
            theme_minimal() + 
            ggtitle("Plot of all labeled Valid data points") 
    })
    
    output$relation3 <- renderPlot({
        df %>% filter(OUTLIER_Present. == 1) %>%
            ggplot(aes_string(x = input$var1, y = input$var2)) +
            geom_point(size = .5) +
            geom_smooth(method = lm, se=TRUE) +
            theme_minimal()+ 
            ggtitle("Plot of only labeled Outliers")
    })
    
    output$compare <- renderPlot({
        ggplot(df, aes_string(x = input$var3, y = input$var4, fill = as.factor(df$OUTLIER_Present. )))+
            geom_boxplot() + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90)) +
            scale_fill_discrete(name = 'Outlier Present',
                                breaks = c(0 , 1),
                                labels = c('No Outliers', 'Outliers'))
    })
    
    output$out <- renderPlot({
        df %>% filter(OUTLIER_Present. == 0) %>%
            ggplot(aes_string(x=input$var5)) +
                geom_histogram(color = 'black', fill = 'skyblue') +
            theme_minimal()+ 
            ggtitle("No Outlier Present") 
    })
    
    output$out2 <- renderPlot({
        df %>% filter(OUTLIER_Present. == 1) %>%
            ggplot(aes_string(x=input$var5)) +
                       geom_histogram(color='black', fill = 'red') +
            theme_minimal()+ 
            ggtitle("Outlier Present") 
    })
    
    output$clus <- renderPlot({
        df %>% 
            ggplot(aes_string(x = input$var6, y = input$var7)) +
                geom_point(aes_string(color = input$var8 ),size = .5) +
                theme_minimal()+ 
                ggtitle("Plot of all data points") 
    })
    
    output$Pred <- renderPrint({
        Club <- input$club
        Club_Speed <- as.numeric(input$Club_Speed)
        Club_Speed <- (Club_Speed - mean(df$Club_Speed)) / sd(df$Club_Speed)
        Angle_Of_Attack <- as.numeric(input$Angle_Of_Attack)
        Angle_Of_Attack <- (Angle_Of_Attack - mean(df$Angle_Of_Attack)) / sd(df$Angle_Of_Attack)
        Club_Path <- as.numeric(input$Club_Path)
        Club_Path <- (Club_Path - mean(df$Club_Path)) / sd(df$Angle_Of_Attack)
        Facet_To_Path <- as.numeric(input$Facet_To_Path)
        Facet_To_Path <- (Facet_To_Path - mean(df$Facet_To_Path)) / sd(df$Facet_To_Path)
        Lie <- as.numeric(input$Lie)
        Lie <- (Lie - mean(df$Lie)) / sd(df$Lie)
        Loft <- as.numeric(input$Loft)
        Loft <- (Loft - mean(df$Loft)) / sd(df$Loft)
        Closure_Rate <- as.numeric(input$Closure_Rate)
        Closure_Rate <- (Closure_Rate - mean(df$Closure_Rate)) / sd(df$Closure_Rate)
        Face_Impact_Lateral <- as.numeric(input$Face_Impact_Lateral)
        Face_Impact_Lateral <- (Face_Impact_Lateral - mean(df$Face_Impact_Lateral)) / sd(df$Face_Impact_Lateral)
        Face_Impact_Vertical <- as.numeric(input$Face_Impact_Vertical)
        Face_Impact_Vertical <- (Face_Impact_Lateral - mean(df$Face_Impact_Vertical)) / sd(df$Face_Impact_Vertical)
        #build data frame
        OC <- as.data.frame(df$Club)
        colnames(OC) <- "Club"
        Club <- as.data.frame(Club)
        dummy2 <- dummyVars(' ~ .', data = OC)
        data <- data.frame(predict(dummy2, newdata = Club))
        data$Club_Speed <- Club_Speed
        data$Angle_Of_Attack <- Angle_Of_Attack
        data$Club_Path <- Club_Path
        data$Facet_To_Path <- Facet_To_Path
        data$Lie <- Lie
        data$Loft <- Loft
        data$Closure_Rate <- Closure_Rate
        data$Face_Impact_Lateral <- Face_Impact_Lateral
        data$Face_Impact_Vertical <- Face_Impact_Vertical
        #prediction variables
        data$Ball_Speed <- 0
        data$Launch_Angle <- 0
        data$Side_Angle <- 0
        data$Backspin <- 0
        data$Sidespin <- 0
        data$Tilt_Angle <- 0
        data$Total_Spin <- 0
        data$Carry_Yards <- 0
        data$Total_Yards <- 0
        data$Offline_Yards <- 0
        data$Descent_Angle <- 0
        data$Peak_Height <- 0
        data$Offline_Yards_Absolute <- 0
        #predict ball speed
        ball_x <- data.matrix(data[,-c(63:75)])
        ball_y <- data[,63]
        xgb_test <- xgb.DMatrix(data = ball_x, label = ball_y)
        ballPred <- predict(ballSpeedFit, xgb_test)
        unscale <- unscale(ballPred, scale(df$Ball_Speed))
        cat("Ball Speed: ", unscale, sep = "\n")
        #predict launch angle
        LA_x <- data.matrix(data[,-c(63:75)])
        LA_y <- data[,64]
        xgb_test <- xgb.DMatrix(data = LA_x, label = LA_y)
        LAPred <- predict(launchAngleFit, xgb_test)
        unscale <- unscale(LAPred, scale(df$Launch_Angle))
        cat("Launch Angle: ", unscale, sep = '\n')
        #predict side angle 
        SA_x <- data.matrix(data[,-c(63:75)])
        SA_y <- data[,65]
        xgb_test <- xgb.DMatrix(data = SA_x, label = SA_y)
        SAPred <- predict(sideAngleFit, xgb_test)
        unscale <- unscale(SAPred, scale(df$Side_Angle))
        cat("Side Angle: ", unscale, sep = '\n')
        #predict back spin
        BS_x <- data.matrix(data[,-c(63:75)])
        BS_y <- data[,66]
        xgb_test <- xgb.DMatrix(data = BS_x, label = BS_y)
        BSPred <- predict(backspinFit, xgb_test)
        unscale <- unscale(BSPred, scale(df$Backspin))
        cat("Backspin: ", unscale, sep = '\n')
        #predict sidespin
        SS_x <- data.matrix(data[,-c(63:75)])
        SS_y <- data[,67]
        xgb_test <- xgb.DMatrix(data = SS_x, label = SS_y)
        SSPred <- predict(sidespinFit, xgb_test)
        unscale <- unscale(SSPred, scale(df$Sidespin))
        cat("Sidespin: ", unscale, sep = '\n')
        #predict tilt angle
        TA_x <- data.matrix(data[,-c(63:75)])
        TA_y <- data[,68]
        xgb_test <- xgb.DMatrix(data = TA_x, label = TA_y)
        TAPred <- predict(tiltAngleFit, xgb_test)
        unscale <- unscale(TAPred, scale(df$Tilt_Angle))
        cat("Tilt Angle: ", unscale, sep = '\n')
        #predict total spin
        TS_x <- data.matrix(data[,-c(63:75)])
        TS_y <- data[,69]
        xgb_test <- xgb.DMatrix(data = TS_x, label = TS_y)
        TSPred <- predict(tiltAngleFit, xgb_test)
        unscale <- unscale(TSPred, scale(df$Total_Spin))
        cat("Total Spin: ", unscale, sep = '\n')
        #predict Carry Yards
        CY_x <- data.matrix(data[,-c(63:75)])
        CY_y <- data[,70]
        xgb_test <- xgb.DMatrix(data = CY_x, label = CY_y)
        CYPred <- predict(carryYardsFit, xgb_test)
        unscale <- unscale(CYPred, scale(df$Carry_Yards))
        cat("Carry Yards: ", unscale, sep = '\n')
        #predict Total Yards
        TY_x <- data.matrix(data[,-c(63:75)])
        TY_y <- data[,71]
        xgb_test <- xgb.DMatrix(data = TY_x, label = TY_y)
        TYPred <- predict(totalYardsFit, xgb_test)
        unscale <- unscale(TYPred, scale(df$Total_Yards))
        cat("Total Yards: ", unscale, sep = '\n')
        #predict offline yards
        OY_x <- data.matrix(data[,-c(63:75)])
        OY_y <- data[,71]
        xgb_test <- xgb.DMatrix(data = OY_x, label = OY_y)
        OYPred <- predict(offlineYardsFit, xgb_test)
        unscale <- unscale(OYPred, scale(df$Offline_Yards))
        cat("Offline Yards: ", unscale, sep = '\n')
        #predict descent angle
        DA_x <- data.matrix(data[,-c(63:75)])
        DA_y <- data[,73]
        xgb_test <- xgb.DMatrix(data = DA_x, label = DA_y)
        DAPred <- predict(descentAngleFit, xgb_test)
        unscale <- unscale(DAPred, scale(df$Descent_Angle))
        cat("Descent Angle: ", unscale, sep = '\n')
        #Peak Height
        PH_x <- data.matrix(data[,-c(63:75)])
        PH_y <- data[,74]
        xgb_test <- xgb.DMatrix(data = PH_x, label = PH_y)
        PHPred <- predict(peakHeightFit, xgb_test)
        unscale <- unscale(PHPred, scale(df$Peak_Height))
        cat("Peak Height: ", unscale, sep = '\n')
        #predict offline yards absolute
        OYA_x <- data.matrix(data[,-c(63:75)])
        OYA_y <- data[,75]
        xgb_test <- xgb.DMatrix(data = OYA_x, label = OYA_y)
        OYAPred <- predict(offlineYardsAbsoluteFit, xgb_test)
        unscale <- unscale(OYAPred, scale(df$Offline_Yards_Absolute))
        cat("Offline Yards Absolute: ", unscale, sep = '\n')
    }
    )
    
    
}