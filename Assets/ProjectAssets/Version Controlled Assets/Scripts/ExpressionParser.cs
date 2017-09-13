using System.Collections;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Linq;
using UnityEngine;
using System;

public static class ExpressionParser {

    static string splitPattern = @"(log\()|(a|\barc)?(cos|sin|tan)h?\(|abs\(|[\[\]]|[\--9]|[α - ω]|pi|[\(-\+\^]|[a-z]";
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
