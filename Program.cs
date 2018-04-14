using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApp10
{
    class Program
    {
        static void Main(string[] args)
        {
            string a = @"C:\folder\plik.R";
            string b = @"C:\Program Files\R\R-3.4.4\bin\Rscript.exe";
            string c = "10 4"; //argumenty

            try
            {
                Console.WriteLine(RScriptRunner.RunCmd(a, b, c));
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
            Console.ReadLine();
        }
    }
}
