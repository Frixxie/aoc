module Main (main) where

data ElfSection = ElfSection Int Int

checkFullyContained :: ElfSection -> ElfSection -> Bool
checkFullyContained (ElfSection start1 end1) (ElfSection start2 end2) =
  (start1 >= start2 && end1 <= end2) || (start2 >= start1 && end2 <= end1)

checkPartialOverlap :: ElfSection -> ElfSection -> Bool
checkPartialOverlap (ElfSection start1 end1) (ElfSection start2 end2) =
  not (end1 < start2 || end2 < start1)

-- example 2-5 -> elfsection 2 5
-- example 11-17 -> elfsection 11 17
parseElfSection :: String -> Maybe ElfSection
parseElfSection str =
  case span (/= '-') str of
    (startStr, '-' : endStr) ->
      case (reads startStr :: [(Int, String)], reads endStr :: [(Int, String)]) of
        ([(start, "")], [(end, "")]) -> Just (ElfSection start end)
        _ -> Nothing
    _ -> Nothing

parseLine :: String -> (ElfSection, ElfSection)
parseLine line =
  case span (/= ',') line of
    (firstStr, ',' : secondStr) ->
      case (parseElfSection firstStr, parseElfSection secondStr) of
        (Just elf1, Just elf2) -> (elf1, elf2)
        _ -> error "Invalid elf section format"
    _ -> error "Invalid line format"

readElfEections :: String -> IO [(ElfSection, ElfSection)]
readElfEections filePath = do
  contents <- readFile filePath
  let (frst, send) = unzip $ map parseLine (lines contents)
  return $ zip frst send

main :: IO ()
main = do
  elfSections <- readElfEections "input.txt"
  let fullyContainedCount = length [() | (elf1, elf2) <- elfSections, checkFullyContained elf1 elf2]
  print fullyContainedCount
  let partialOverlapCount = length [() | (elf1, elf2) <- elfSections, checkPartialOverlap elf1 elf2]
  print partialOverlapCount
