using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using UnityEngine;

public class MaximaTest : MonoBehaviour {

    public static List<string> Functions = new List<string> { "pow(x, y) := x^y" };

    // Use this for initialization
    void Start () {
        print("(((3 * (x ^ 2)) + (2 * x)) + 1)");
        print(Simplify("(((3 * (x ^ 2)) + (2 * x)) + 1)"));
    }

    private static Process Process = NewMaxima();

    private static Process NewMaxima()
    {
        string MaximaRoot = System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), @"Assets\ThirdPartyAssets\Maxima-5.30.0");
        string MaximaExe = System.IO.Path.Combine(MaximaRoot, @"lib\maxima\5.30.0\binary-gcl\maxima.exe");
        string WorkingDirectory1 = System.IO.Path.Combine(MaximaRoot, @"bin\");
        print(MaximaRoot);
        print(MaximaExe);
        print(WorkingDirectory1);

        print(System.IO.Directory.GetCurrentDirectory());

        if (Process != null)
        {
            Process.Dispose();
        }
        var startInfo = new ProcessStartInfo(MaximaExe, @" - eval ""(cl-user::run)"" -f ")
        {
            WorkingDirectory = WorkingDirectory1,
            UseShellExecute = false,
            //CreateNoWindow = true,
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true
        };
        //startInfo.EnvironmentVariables.Add("maxima_prefix", @"..\..\Maxima-5.30.0");
        startInfo.EnvironmentVariables.Add("maxima_prefix", MaximaRoot);
        print(startInfo.EnvironmentVariables["maxima_prefix"]);
        return Process.Start(startInfo);
    }

    private static string runExpression()
    {

    }

    #region StringOps
    public static string Eval(string expr)
    {
        if (Process == null)
        {
            print("Starting maxima");
            Process = NewMaxima();
        }
        print("Starting eval");
        Process.StandardInput.WriteLine(string.Format("{0}$ grind({1});", string.Join("$", Functions.ToArray()), expr.ToLower()));
        print("printing standardInput");
        string result = Process.StandardOutput.ReadLine();
        print(result);
        while (!result.EndsWith("$"))
        {
            result += Process.StandardOutput.ReadLine();
            print("Partial result");
            print(result);
        }
        Process.StandardOutput.ReadLine();
        if (result.EndsWith("$"))
        {
            print("Non failure eval");
            return result.TrimEnd('$');
        }
        print("Failure eval");
        Process = NewMaxima();
        throw new System.Exception(string.Format("Unexpected result: {0}", result));
    }
    public static string Simplify(string expr)
    {
        return Eval(string.Format("factor(fullratsimp(trigsimp({0})))", expr));
    }
    public static string Differentiate(string expr, string wrt = "x")
    {
        return Eval(string.Format("diff({0}, {1})", expr, wrt));
    }
    public static string Integrate(string expr, string wrt = "x")
    {
        return Eval(string.Format("integrate({0}, {1})", expr, wrt));
    }
    public static string Integrate(string expr, int from, int to, string wrt = "x")
    {
        return Eval(string.Format("integrate({0}, {1}, {2}, {3})", expr, wrt, from, to));
    }
    #endregion


    // Update is called once per frame
    void Update () {
		
	}
}

