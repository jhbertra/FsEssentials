open System.IO

let files = 
    Directory.GetFiles("FsEssentials") 
    |> List.ofSeq
    |> List.filter (fun s -> s.EndsWith(".fs"))

let rec resolveTypeclassDeps tc =
    let deps =
        File.ReadLines("FsEssentials/TypeclassSnippets/" + tc + ".fs") 
        |> List.ofSeq
        |> List.map (fun s -> s.Trim())
        |> List.filter (fun s -> s.StartsWith("// #Inherit")) 
        |> List.map (fun s -> s.Substring(12))
    deps @ List.collect resolveTypeclassDeps deps
    |> List.distinct

let resolveTypeclass tc =
    let typeclasses = (tc :: resolveTypeclassDeps tc) |> List.rev |> Seq.ofList
    seq {
        for typeclass in typeclasses do
            yield!
                File.ReadLines("FsEssentials/TypeclassSnippets/" + typeclass + ".fs") 
                |> List.ofSeq
                |> List.skipWhile (fun line -> not (line.Contains("// #Typeclass " + typeclass)))
                |> List.filter (fun s -> not (s.StartsWith("// #Inherit")))
                |> List.map ((+) "    ")
                |> Seq.ofList
    } |> List.ofSeq

for file in files do
    let lines = File.ReadLines(file) |> List.ofSeq
    let typeclass = 
        lines 
        |> List.map (fun s -> s.Trim())
        |> List.tryFind (fun s -> s.StartsWith("// #Instance")) 
        |> Option.map (fun s -> s.Substring(13))
    
    Option.map(fun tc -> 
        let linesToInsert = resolveTypeclass tc
        let (before,_) = List.splitAt ((List.findIndex (fun (s : string) -> s.Trim() = "// #Instance " + tc) lines) + 1) lines
        let (_,after) = List.splitAt (List.findIndex (fun (s : string) -> s.Trim() = "// #Instance End") lines) lines
        File.WriteAllLines(file, before @ ("" :: linesToInsert) @ ("" :: after))
        ) typeclass
        |> ignore
    