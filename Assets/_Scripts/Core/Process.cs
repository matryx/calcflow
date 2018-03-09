using UnityEngine;

using System;
using System.Text;
using System.Threading;

namespace Nanome.Core
{

    public class Process
    {

        public class Result
        {
            public bool success = false;
            public int code = 0;
            public string output = null;
            public string error = null;

            public string execPath = null;
            public string execArgs = null;
            public string execDir = null;
        }

        public delegate void ExecDelegate(Process.Result result);
        public static void execInThread(string path, string args, string execDir, ExecDelegate callback, int timeout = 1000 * 100)
        {
            Async main = Async.runInThread(delegate (Async thread)
            {
                // Process result report class
                Process.Result res = new Process.Result();
                res.success = false;
                res.code = -1;
                res.output = "";
                res.error = "";
                res.execPath = path;
                res.execArgs = args;
                res.execDir = execDir;
                // Create the process object
                using (var process = new System.Diagnostics.Process())
                {
                    // Set process options
                    process.StartInfo.FileName = path;
                    process.StartInfo.Arguments = args;
                    process.StartInfo.WorkingDirectory = execDir;
                    process.StartInfo.CreateNoWindow = true;
                    process.StartInfo.UseShellExecute = false;
                    process.StartInfo.RedirectStandardOutput = true;
                    process.StartInfo.RedirectStandardError = true;
                    // Prepare for output reading
                    var output = new StringBuilder();
                    var error = new StringBuilder();
                    // Set the output data pipes and callback
                    using (AutoResetEvent outputWaitHandle = new AutoResetEvent(false))
                    using (AutoResetEvent errorWaitHandle = new AutoResetEvent(false))
                    {
                        // Output buffer events
                        process.OutputDataReceived += (sender, e) =>
                        {
                            if (e.Data == null)
                            {
                                outputWaitHandle.Set();
                            }
                            else
                            {
                                output.AppendLine(e.Data);
                            }
                        };
                        // Error buffer event
                        process.ErrorDataReceived += (sender, e) =>
                        {
                            if (e.Data == null)
                            {
                                errorWaitHandle.Set();
                            }
                            else
                            {
                                error.AppendLine(e.Data);
                            }
                        };
                        // Actually start the process
                        process.Start();
                        // Start reading stderr/stdout
                        process.BeginOutputReadLine();
                        process.BeginErrorReadLine();
                        // Wait until correctly exited
                        var processDone = process.WaitForExit(timeout);
                        var outputDone = outputWaitHandle.WaitOne(timeout);
                        var errorDone = errorWaitHandle.WaitOne(timeout);
                        // Check result
                        if (processDone && outputDone && errorDone)
                        {
                            // Process completed.
                            res.code = process.ExitCode;
                        }
                        else
                        {
                            // Timed out.
                            res.code = -1;
                        }
                        // Read results
                        res.success = res.code == 0;
                        res.output = output.ToString();
                        res.error = error.ToString();
                    }
                }
                // Done
                thread.pushEvent("ExecDone", res);
            });
            main.onEvent("ExecDone", delegate (object datas)
            {
                Process.Result res = (Process.Result)datas;
                callback(res);
            });
        }

    }

}