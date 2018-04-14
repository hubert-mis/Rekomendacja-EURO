using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApp10
{
    public class RScriptRunner
    {
        /// <summary>
        /// Wywyołuje skrypt R przy użyciu Rscript.exe
        /// </summary>
        /// <param name="rCodeFilePath">Pełna ścieżka do wywoływanego skryptu R</param>
        /// <param name="rScriptExecutablePath">Pełna ścieżka "Rscript.exe" </param>
        /// <param name="args">Argumenty dla skryptu R oddzielone spacjami</param>
        /// <returns>Zwraca string wyniku wywołania skryptu R</returns>
        public static string RunCmd(string rCodeFilePath, string rScriptExecutablePath, string args)
        {
            string file = rCodeFilePath;
            string result = string.Empty;

            try
            {
                var info = new ProcessStartInfo();
                info.FileName = rScriptExecutablePath;
                info.WorkingDirectory = Path.GetDirectoryName(rScriptExecutablePath);
                info.Arguments = "--vanilla " + rCodeFilePath + " " + args;

                info.RedirectStandardInput = false;
                info.RedirectStandardOutput = true;
                info.UseShellExecute = false;
                info.CreateNoWindow = true;

                using (var proc = new Process())
                {
                    proc.StartInfo = info;
                    proc.Start();
                    result = proc.StandardOutput.ReadToEnd();
                }
                return result;
            }
            catch (Exception ex)
            {
                throw new Exception("R Script failed: " + result, ex);
            }
        }
    }
}
