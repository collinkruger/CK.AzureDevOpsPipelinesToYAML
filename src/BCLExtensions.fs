module BCLExtensions

module FileExt =
    open System.IO

    let readFile fp =
        File.ReadAllText(fp)

    let writeFile fp str =
        File.WriteAllText(fp, str)

module StringExt =
    open System
    open System.Text.RegularExpressions

    let  join (seperator:string) (lines:string seq) =
        String.Join(seperator, lines)

    let replace (pattern:string) (replacement:string) (str:string) =
        Regex.Replace(str, pattern, replacement)