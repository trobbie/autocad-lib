using Microsoft.VisualStudio.TestTools.UnitTesting;
using AABase.Logic;
using System;

namespace AAExt1.Tests
{
    [TestClass]
    public class UtilityTests
    {
        static void TestFindPrecisionNumber(double arg, int expected)
        {
            int test = Utility.FindPrecisionOfNumber(arg);
            if (test != expected)
                Assert.Fail($"Arg {arg} returned {test}.  Expected {expected}.");
        }

        static void TestIsAngleExclusivelyBetwStartAndEndAngles(double thisAngle, double startAngle, double endAngle, bool expected)
        {
            bool test = Utility.IsAngleExclusivelyBetwStartAndEndAngles(thisAngle, startAngle, endAngle);
            if (test != expected)
                Assert.Fail($"Arg ({thisAngle},{startAngle},{endAngle}) returned {test}.  Expected {expected}.");
        }

        [TestMethod]
        public void FindPrecisionOfNumber_TEST1()
        {
            TestFindPrecisionNumber(3, 0);
        }
        [TestMethod]
        public void FindPrecisionOfNumber_TEST2()
        {
            TestFindPrecisionNumber(30, 0);
        }
        [TestMethod]
        public void FindPrecisionOfNumber_TEST3()
        {
            TestFindPrecisionNumber(3.0, 0);
        }
        [TestMethod]
        public void FindPrecisionOfNumber_TEST4()
        {
            TestFindPrecisionNumber(0.3, 1);
        }
        [TestMethod]
        public void FindPrecisionOfNumber_TEST5()
        {
            TestFindPrecisionNumber(3.10, 1);
        }
        [TestMethod]
        public void FindPrecisionOfNumber_TEST6()
        {
            TestFindPrecisionNumber(3.01, 2);
        }
        [TestMethod]
        public void FindPrecisionOfNumber_TEST7()
        {
            TestFindPrecisionNumber(3.104, 3);
        }
        [TestMethod]
        public void FindPrecisionOfNumber_TEST8()
        {
            TestFindPrecisionNumber(3.104300, 4);
        }
        [TestMethod]
        public void FindPrecisionOfNumber_TEST9()
        {
            TestFindPrecisionNumber(3.104333, 6);
        }


        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST01() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.5, Math.PI*0.49, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST02() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.49, Math.PI*0.5, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST03() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.51, Math.PI*0.5, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST04() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.01, Math.PI*0.49, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST05() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.49, Math.PI*1.99, true); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST06() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.49, Math.PI*0.01, true); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST07() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.51, Math.PI*0.01, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST08() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.51, Math.PI*0.49, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST09() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.51, Math.PI*1.99, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST10() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*0.99, Math.PI*0.51, true); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST11() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*1.49, Math.PI*0.49, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST12() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*1.49, Math.PI*0.51, true); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST13() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*1.99, Math.PI*0.49, false); }
        [TestMethod] public void IsAngleExclusivelyBetwStartAndEndAngles_TEST14() { TestIsAngleExclusivelyBetwStartAndEndAngles(Math.PI*0.5, Math.PI*1.99, Math.PI*0.51, true); }
        

    }
}