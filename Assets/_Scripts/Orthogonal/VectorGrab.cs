using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VectorGrab : MonoBehaviour
{

    public Transform vec1;

    public OrthPtManager ptManager;
    public OrthPtOutputMenu ptOutputMenu;

    public ConstraintGrabbable pt1Grabber;

    void Start()
    {
        // pt1Grabber.lastLocalPos = point1.localPosition;
        // grabbingPoint(point1, pt1Grabber);
        // pt2Grabber.lastLocalPos = point2.localPosition;
        // grabbingPoint(point2, pt2Grabber);
        // pt3Grabber.lastLocalPos = point3.localPosition;
        // grabbingPoint(point3, pt3Grabber);
    }
    void Update()
    {
        if (!pt1Grabber.IsGrabbed) pt1Grabber.lastLocalPos = vec1.localPosition;
        else grabbingPoint(vec1, pt1Grabber);
    }

    private void grabbingPoint(Transform point, ConstraintGrabbable grabber)
    {
        Vector3 newLoc = Vector3.zero;
        //if (FixedPlane && presentPlane.forwardPlane.GetComponent<MeshRenderer>().enabled)
        //{
        //    newLoc = Vector3.ProjectOnPlane(grabber.lastLocalPos - presentPlane.centerPt.localPosition, presentPlane.lookAtTarget.localPosition - presentPlane.plane.localPosition);
        //    newLoc = newLoc + presentPlane.centerPt.localPosition;
        //    if (newLoc.x > 10 || newLoc.x < -10 || newLoc.y > 10 || newLoc.y < -10 || newLoc.z > 10 || newLoc.z < -10)
        //    {
        //        grabber.transform.position = point.position;
        //    }
        //    point.localPosition = newLoc;
        //}
        //else
        //{
            newLoc = grabber.lastLocalPos;
            point.localPosition = newLoc;
        //}
        ptManager.updatePoint(point.name, newLoc);
    }
}
