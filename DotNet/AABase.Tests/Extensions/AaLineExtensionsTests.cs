using Microsoft.VisualStudio.TestTools.UnitTesting;
using AABase.Logic;
using System.Collections.Generic;
using System.Linq;

namespace AABase.Tests
{
  [TestClass]
    public class AaLineExtensionsTests
    {
        static void TestGetLineList(IEnumerable<IEntity>listObjects, List<AaLine> expected)
        {
            // Act
            IEnumerable<AaLine> test = listObjects.GetLineList();
            // Assert
            if (!test.OrderBy(t => t).SequenceEqual<AaLine>(expected.OrderBy(e => e)))
                Assert.Fail($"Failed args {listObjects}.\nReturned {test.ToStringDebug()}.\nExpected {expected.ToStringDebug()}.");
        }

        [TestMethod]
        public void GetLineList_TEST1()
        {
            List<FakeEntity> listLinesEntities = new List<FakeEntity>();

            // triangle (as individual lines)
            listLinesEntities.Add(new FakeLineEntity(0, 0, 10, 0));
            listLinesEntities.Add(new FakeLineEntity(10, 0, 10, 10));
            listLinesEntities.Add(new FakeLineEntity(10, 10, 0, 0));

            List<AaLine> listLinesExpected = new List<AaLine>();
            listLinesExpected.Add(new AaLine(new AaPoint3d(0,0), new AaPoint3d(10,0)));
            listLinesExpected.Add(new AaLine(new AaPoint3d(10,0), new AaPoint3d(10,10)));
            listLinesExpected.Add(new AaLine(new AaPoint3d(10,10), new AaPoint3d(0,0)));

            TestGetLineList(listLinesEntities, listLinesExpected);
        }

    }
    
}