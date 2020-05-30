plots_extra = function(result, features = NULL, row.ids = NULL, type = "parallel", grid.size = 50L) {
    assert_true(type %in% c("icearea", "spider", "parallel"))
    assert_character(features, null.ok = TRUE, min.len = 2L)
    assert_numeric(row.ids, null.ok = TRUE)
    assert_class(credit.cf, c("Counterfactuals", "InterpretationMethod"))
    
    if (is.null(features)) {
        features = result$predictor$data$feature.names
    }
    
    cf = result$results$counterfactuals.diff
    cf = result$results$counterfactuals
    
    if (type %in% c("parallel", "spider")) {
        if (!is.null(row.ids)) {
            cf = cf[row.ids,]
        } else {
            row.ids = 1:nrow(cf)
        }
        cf = cf[, features]
        cf = rbind(cf, result$x.interest[, features]) ## add x.original
        char.id = sapply(cf, is.character)
        cf[, char.id] = sapply(cf[, char.id], as.numeric)
        
        factor.id = sapply(cf, is.factor)
        cf[, factor.id] = sapply(cf[, factor.id], unclass) 
        
        mycolors = c(gray.colors(nrow(cf)-1, start = 0.2, end = 0.8, gamma = 2.2), "blue")
        names(mycolors) <- rownames(cf)
    }
    if (type == "parallel") {
        cf$row.names = rownames(cf)
        param.set = result$.__enclos_env__$private$param.set
        val = getValues(param.set)
        val.l = lapply(val, function(x) unlist(x, use.names = FALSE))
        colscale = scale_colour_manual(name = "rows",values = mycolors)
        p = ggparcoord(cf, columns = 1:length(features), groupColumn = "row.names", 
            scale = "uniminmax", showPoints = TRUE) + 
            #scale_colour_grey(start = 0.7, end = 0.0) + 
            theme_bw() +
            ylim(c(-0.1, 1.1)) +
            theme(legend.position="none") +
            ylab("Scaled feature values") +
            geom_point(aes()) +
            colscale
        for (i in 1:length(features)) {
            # p = p + geom_vline(aes(xintercept = i), colour = "grey")
            if (features[i] %in% names(val.l)) {
                max = tail(val.l[[features[i]]], 1)
                min = head(val.l[[features[i]]], 1)
            } else {
                max = round(max(cf[features[i]]), 3)
                min = round(min(cf[features[i]]), 3)
            }
            p = p + geom_text(aes_string(label = max, x = i-0.07, y = 1.05), colour = "black")
            p = p + geom_text(aes_string(label = min, x = i-0.07, y = -0.05), colour = "black")
        }
        return(p) 
    }
    
    if (type == "spider") {
        p = radarchart(cf, maxmin = FALSE, plwd = c(rep(1, nrow(cf)-1), 2), axistype = 2,
            plty = 1, 
            pcol = mycolors, 
            axislabcol = "black")
    }
    
    if (type == "icearea") {
        if (!is.null(row.ids)) {
            plots = lapply(row.ids, function(i) {
                res = get_ice_curve_area(instance = cf[i, ], features = features, 
                    predictor = result$predictor, param.set = result$.__enclos_env__$private$param.set, 
                    grid.size = grid.size)
                p = plot_ice_curve_area(res, predictor = result$predictor, cf[i, ])
            })
            if (length(plots) > 1) {
                layout = get_layout(length(row.ids), NULL, NULL)
                # Fill gtable with graphics
                p =  marrangeGrob(grobs = plots, nrow = layout$nrows, ncol = layout$ncols, 
                    top = NULL) 
            } else {
                p = plots[[1]]
            }
        } else {
            change.id = which(rowSums(result$results$counterfactuals.diff[, features] != 0) == cf$nr.changed)
            instances = cf[change.id, ]
            res = get_ice_curve_area(instance = instances[1, ], features = features, 
                predictor = result$predictor, param.set = result$.__enclos_env__$private$param.set, 
                grid.size = grid.size)
            p = plot_ice_curve_area(res, predictor = result$predictor, instances, 
                x.interest = result$x.interest[, features])
        }
    return(p)
    }
}


get_ice_curve_area = function (instance, features, predictor, param.set, grid.size) {
    instance =  instance[, 1:predictor$data$n.features]
    min.max = as.data.frame(rbind(getLower(param.set),
        getUpper(param.set)))
    
    val = getValues(param.set)
    val$use.orig = NULL
    val.l = lapply(val, function(x) unlist(x, use.names = FALSE))
    values = c(as.list(min.max), val.l)
    
    
    grids = sapply(features, function(feat){
        # get min max values
        val = values[[feat]]
        
        # make grid of one feature
        grid = iml:::get.grid.1D(feature = val, grid.size = grid.size, 
            type = "equidistant")
        return(as.data.frame(grid))
    })
    expgrid = expand.grid(a = grids[[1]], b = grids[[2]])
    colnames(expgrid) = features
    
    instance = instance[, !names(instance) %in% features]
    instance.df = instance[rep(row.names(instance), nrow(expgrid)), ]
    
    grid.df = cbind.data.frame(instance.df, expgrid)
    
    # predict outcomes
    pred = predictor$predict(newdata = grid.df)[[1]]
    results = cbind(expgrid, pred)
    
    return(results)
}

plot_ice_curve_area = function(grid, predictor, instance = NULL, x.interest = NULL) {
    nams = names(grid)[1:2]
    if (all(predictor$data$feature.types[nams] %in% "numerical") | 
            all(predictor$data$feature.types[nams] %in% "categorical")) {
        p = ggplot(grid, mapping = aes_string(x = nams[1], 
            y = nams[2])) + geom_tile(aes(fill = pred)) +
            theme_bw() +
        stat_contour(aes(z = pred), colour = "white") +
            geom_point(data = instance, aes_string(x=nams[1], y=nams[2]), colour = "black") +
          geom_text_contour(aes(z = pred), colour = "white")
        # geom_rug(data = predictor$data$get.x(), aes_string(x = nams[1], y = nams[2]), 
        #     alpha = 0.2, sides = "bl",
        #     position = position_jitter(width = 0.2, height = 0.1))
        if (!is.null(x.interest)) {
            p = p + geom_point(aes(x = x.interest[1,1], y = x.interest[1,2]), colour = "white") 
        }
    } else {
        categorical.feature = nams[predictor$data$feature.types[nams] =="categorical"]
        numerical.feature = setdiff(nams[1:2], categorical.feature)
        p = ggplot(grid, mapping = aes_string(x = numerical.feature, y = "pred")) + 
            geom_line(aes_string(group = categorical.feature, color = categorical.feature)) +
            geom_point(aes(x = cf[1,numerical.feature], y = instance$pred), colour = "black") +
            geom_label(label = "counterfactual", aes(x = cf[1,numerical.feature]+8, y = instance$pred), colour = "black")
    }
    p = ggMarginal(p, data = predictor$data$get.x(), x = nams[1], y = nams[2], type = "histogram")
    return(p)
}


plot_hv = function(result, ylim = c(0.7, 0.9)) {
        df = result$log
        singlep = ggplot(df, aes(generation, fitness.domHV)) + 
            geom_line() + 
            theme_bw() +
            ylim(ylim[1], ylim[2]) +
            ylab("domhv")
        return(singlep)
}
