using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using UnityEngine;

public class MaximaTest : MonoBehaviour {

    public static List<string> Functions = new List<string> { "pow(x, y) := x^y" };

    // Use this for initialization
    void Start () {
        print("(((3 * (x ^ 2)) + (2 * x)) + 1)");
        runExpression();
        Simplify("(((3 * (x ^ 2)) + (2 * x)) + 1)");
    }

    static string MaximaRoot = System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), @"Assets\ThirdPartyAssets\Maxima-5.30.0");
    static string MaximaPath = System.IO.Path.Combine(MaximaRoot, @"bin");
    static string MaximaExe = System.IO.Path.Combine(MaximaPath, @"maxima.bat");
    static string inputFile =  @"testInput.txt";
    static string args = "- eval \"(cl-user::run)\" --very-quiet  < \"" + inputFile + "\"";
    static string settings = "display2d: false;";
    

    private static Process Process = null;

    private static Process NewMaxima()
    {
        print(MaximaRoot);
        print(MaximaExe);
        //print(WorkingDirectory1);

        print(System.IO.Directory.GetCurrentDirectory());

        if (Process != null)
        {
            Process.Dispose();
        }
        var startInfo = new ProcessStartInfo(MaximaExe, @" - eval ""(cl-user::run)"" -f ")
        {
            //WorkingDirectory = WorkingDirectory1,
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

    private static void runExpression()
    {
        print("./maxima " + args);
        Nanome.Core.Process.execInThread(MaximaExe, args, MaximaPath, callback);
    }

    private static void callback(Nanome.Core.Process.Result result)
    {
        print(result.output);
        print(result.error);
    }

    #region StringOps
    public static void Eval(string expr)
    {
        System.IO.File.WriteAllText(System.IO.Path.Combine(MaximaPath, inputFile), settings + expr + ";");
    }
    public static void Simplify(string expr)
    {
        Eval(string.Format("factor(fullratsimp(trigsimp({0})))", expr));
    }
    public static void Differentiate(string expr, string wrt = "x")
    {
        Eval(string.Format("diff({0}, {1})", expr, wrt));
    }
    public static void Integrate(string expr, string wrt = "x")
    {
        Eval(string.Format("integrate({0}, {1})", expr, wrt));
    }
    public static void Integrate(string expr, int from, int to, string wrt = "x")
    {
        Eval(string.Format("integrate({0}, {1}, {2}, {3})", expr, wrt, from, to));
    }
    #endregion


    // Update is called once per frame
    void Update () {
		
	}
}

