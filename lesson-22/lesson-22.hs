--QC22-1
qc22_1_Main :: IO ()
qc22_1_Main = do
  vals <- mapM (\ _ -> getLine) [1..3]
  mapM_ putStrLn vals

--QC22-2
myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n ioact = mapM (\ _ -> ioact) [1..n]
