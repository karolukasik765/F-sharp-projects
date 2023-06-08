open System
open System.IO 


let StockPricesSimulation(count_N: int, steps: int, price_S_0: float,drift: float, vol: float, years: float, seed:int) =

    //defining names of variables in mathematical convention
    let n_steps = float(steps+steps%2) //ensuring even number of steps required for boxMullerTransform
    let r = drift
    let t = years
    let sigma = vol //volatility
    
    //function generating list of n uniformly distributed numbers in range from 0 to 1 for fixed seed
    let generateNDoubleRandoms(seed: int, n: int)=

        let fix_rand_obj:Random = new Random(seed)

        let rec generate( i ,random_numbers )=
            if i=n then random_numbers
            else generate(i+1, (fix_rand_obj.NextDouble())::random_numbers)
        
        generate(0, [])

    //function transforming list of unformly distributed numbers to standard normally distributed using Box-Muller transform
    let rec boxMullerTransform(unif_list) =
        
        match unif_list with
            | u1::u2::tail -> 
                let z0 = sqrt(-2. * log u1) * cos (2. * Math.PI * u2)
                let z1 = sqrt(-2. * log u1) * sin (2. * Math.PI * u2)
                z0::z1::boxMullerTransform(tail)
            | _ -> []

    //function generating list of Brownian Motion steps using standard normally distributed variables and price of stock at time 0
    let rec generateBrownianMotion(stand_norm_list, previous_price:float)=

        match stand_norm_list with
            | h::tail -> 
                let s = previous_price*exp((r-(sigma**2.)/2.)*t/n_steps + sigma*sqrt(t/n_steps)*h)
                s::generateBrownianMotion(tail,s)
            | [] -> []   
    
    //function estimating historical (realized) volatility of the stock using logarithm of the returns
    let realizedVolatility (brow_motion_list:float list)=

        let rec RiListGenerator prices_list s_i= 
            match prices_list with
            | s_i_plus_1::tail -> 
                let r_i = log (s_i_plus_1/s_i)
                r_i::(RiListGenerator tail s_i_plus_1)
            | [] -> []   

        let estimatedRiMean r_i_list  = (List.fold (fun x y -> x + y) 0. r_i_list)/n_steps

        let estVolatility r_i_list =

            let mean = estimatedRiMean r_i_list

            let rec helper sum list =
                match list with 
                | h::tail -> helper (sum+(h-mean)**2.) tail
                | []   -> sum     

            //value of estimated sigma/volatility
            sqrt((n_steps/(t*(n_steps-1.)))*(helper 0. r_i_list))

        estVolatility (RiListGenerator brow_motion_list price_S_0)
    
    //repeating simulation of single stock prices N times, generating N different paths with N estimated volatilities
    //function outputs zipped lists, one containing final stock prices after n steps and second with estimated volatility for each path
    let generateNPaths  = 

        let rec helper i  list_final_prices list_voltalities=
            if i = 0 then List.zip  list_final_prices list_voltalities
            else 
                let i_simulation = generateBrownianMotion(boxMullerTransform(generateNDoubleRandoms(seed+i, int(n_steps))),price_S_0)
                helper (i-1) ((List.head (List.rev i_simulation))::list_final_prices) ((realizedVolatility(i_simulation))::list_voltalities)
        helper count_N [] []    

    //helper function converting simulation results to string
    let rec convertSimulationToString zipped_list=
        match zipped_list with 
        | (f_price,volt)::tail -> string(f_price)+" "+string(volt)+"\n"+ convertSimulationToString tail
        | _ -> ""

    //At the end StockPricesSimulation function saves results to a txt file with one row for each path and 
    //the following columns for each row – Final Stock Price, Realized Volatility
    File.WriteAllText("output.txt", "FinalStockPrice RealizedVolatility\n"+(convertSimulationToString generateNPaths))


StockPricesSimulation(1000, 250, 100,0.05,0.15,1.,19098)



