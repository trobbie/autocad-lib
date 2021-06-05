using Microsoft.VisualStudio.TestTools.UnitTesting;
using AABase.Logic;
using OverlapResultSummary = AABase.Logic.AaGeCurve.OverlapResultSummary;

namespace AABase.Tests
{
  [TestClass]
    public class AaGeCurveTests
    {
        static readonly double a = 0.0001; // slight shift in points that are _outside_ tolerance (should be seen as different)
        static readonly double b = 0.0000001; // slight shift in points that are _within_ tolerance (should be seen as same)
        static void TestFindOverlap(string descTest, IGeCurve thisCurve, IGeCurve otherCurve, OverlapResultSummary expectedSummary)
        {
            // Act
            AaGeCurveOverlapResult test = thisCurve.FindOverlap(otherCurve);
            // Assert
            if (!test.Summary.Equals(expectedSummary))
                Assert.Fail($"Failed test: {descTest}.\nReturned {test.ToString()}.\nExpected {expectedSummary.ToString()}.");
        }

        [TestMethod]
        public void Exact_X_Same()
        { 
            TestFindOverlap("Exact_X_Same",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void Exact_X_SameWithFuzzX()
        { 
            TestFindOverlap("Exact_X_SameWithFuzzX",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10+b,0,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void Exact_X_SameWithFuzzY()
        { 
            TestFindOverlap("Exact_X_SameWithFuzzY",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,b,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void NoOverlap_X_AlmostX()
        { 
            TestFindOverlap("NoOverlap_X_AlmostX",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10+a,a)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void NoOverlap_X_AlmostY()
        { 
            TestFindOverlap("NoOverlap_X_AlmostY",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,a,0)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void NoOverlap_X_TouchingLines()
        { 
            TestFindOverlap("NoOverlap_X_TouchingLines",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        AaGeCurve.Create(new AaPoint3d(10,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.NoOverlap);
        }

        [TestMethod]
        public void NoOverlap_X_Fuzz_Gap()
        { 
            TestFindOverlap("NoOverlap_X_Fuzz_Gap",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        AaGeCurve.Create(new AaPoint3d(10+a,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void EndOverlaps_X_Fuzz_Overlap()
        { 
            TestFindOverlap("EndOverlaps_X_Fuzz_Overlap",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        AaGeCurve.Create(new AaPoint3d(10-a,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.EndOverlapsOtherEnd);
        }
        [TestMethod]
        public void ContainsOther_X_Fuzz1()
        { 
            TestFindOverlap("ContainsOther_X_Fuzz1",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(20,0,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(20-a,0,0)),
                        OverlapResultSummary.ContainsOther);
        }
        [TestMethod]
        public void ContainsOther_X_Fuzz2()
        { 
            TestFindOverlap("ContainsOther_X_Fuzz2",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(20,0,0)),
                        AaGeCurve.Create(new AaPoint3d(a,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.ContainsOther);
        }
        [TestMethod]
        public void ContainedByOther_X_Fuzz1()
        { 
            TestFindOverlap("ContainedByOther_X_Fuzz1",
                        AaGeCurve.Create(new AaPoint3d(a,0,0), new AaPoint3d(20,0,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.ContainedByOther);
        }
        [TestMethod]
        public void ContainedByOther_X_Fuzz2()
        { 
            TestFindOverlap("ContainedByOther_X_Fuzz2",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(20-a,0,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.ContainedByOther);
        }
        [TestMethod]
        public void Exact_Y_Same()
        { 
            TestFindOverlap("Exact_Y_Same",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void Exact_Y_SameWithFuzzX()
        { 
            TestFindOverlap("Exact_Y_SameWithFuzzX",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(b,10,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void Exact_Y_SameWithFuzzY()
        { 
            TestFindOverlap("Exact_Y_SameWithFuzzY",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10+b,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void NoOverlap_Y_AlmostX()
        { 
            TestFindOverlap("NoOverlap_Y_AlmostX",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(a,10,0)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void NoOverlap_Y_AlmostY()
        { 
            TestFindOverlap("NoOverlapY_AlmostY",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(a,10+a,0)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void NoOverlap_Y_TouchingLines()
        { 
            TestFindOverlap("NoOverlap_Y_TouchingLines",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        AaGeCurve.Create(new AaPoint3d(0,10,0), new AaPoint3d(0,20,0)),
                        OverlapResultSummary.NoOverlap);
        }

        [TestMethod]
        public void NoOverlap_Y_Fuzz_Gap()
        { 
            TestFindOverlap("NoOverlap_Y_Fuzz_Gap",
                        AaGeCurve.Create(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        AaGeCurve.Create(new AaPoint3d(0,10+a,0), new AaPoint3d(0,20,0)),
                        OverlapResultSummary.NoOverlap);
        }
    }
    
}