using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Linq;
using UnityEngine;
using System;
using Nanome.Core.Extension;

public static class ExpressionParser {

    static string logPattern = @"log\(";
    static string trigPattern = @"(?:a|\barc)?(?:cos|sin|tan)h?\(";
    static string absPattern = @"abs\(";
    static string sqrBracketPattern = @"[\[\]]";
    static string numberPattern = @"[0-9\.]";
    static string greekPattern = @"[α-ω]|pi";
    static string arithPattern = @"[\(\)\-\+\*\/\^]";
    static string variablePattern = @"[A-Za-z]";
    static string[] patterns = { logPattern, trigPattern, absPattern, sqrBracketPattern, numberPattern, greekPattern, arithPattern, variablePattern };
    static string splitPattern = @"("+patterns.Join("|")+")";
    // Use this for initialization
    static Dictionary<string, string> map = new Dictionary<string, string>() {
            {"a(?=cos|sin|tan)","arc"},
            { "π", "pi"},
            { "\\[", "(" }, {"\\]", ")" },
    };

    static string Clean(string input)
    {
        return map.Aggregate(input, (i, m) => Regex.Replace(i, m.Key, m.Value, RegexOptions.IgnoreCase));
    }

    public static List<string> Parse(this string input) {
        input = Clean(input);
        return Regex.Matches(input, splitPattern).Cast<Match>().Select(m => m.Value).ToList<string>();
    }


}
