using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RayCastSender : MonoBehaviour {

    public class TargetData
    {
        public bool hitting;
        public GameObject targetParent;
        public GameObject target;

        /// <summary>
        /// Basic Constructor
        /// </summary>
        /// <param name="hit"></param>
        /// <param name="target"></param>
        /// <param name="targetParent"></param>
        public TargetData(bool hit, GameObject target, GameObject targetParent)
        {
            this.hitting = hit;
            this.target = target;
            this.targetParent = targetParent;
        }

        public TargetData()
        {
            hitting = false;
            targetParent = null;
            target = null;
        }

    }
    #region publicVariables
    public LayerMask raycastLayers;
    #endregion
    private Vector3 targetPoint;

    public Vector3 TargetPoint
    {
        get
        {
            return targetPoint;
        }
    }



    private TargetData currTargetData = new TargetData();

    public TargetData CurrTargetData
    {
        get
        {
            return currTargetData;
        }
    }


    private void OnDisable()
    {
        NotifyRaycastEnd(currTargetData);
        currTargetData = new TargetData();
        this.targetPoint = Vector3.zero;
    }

    void Update()
    {
        TargetData lastTargetData = currTargetData;
        currTargetData = getTarget();
        if (lastTargetData.target != currTargetData.target)
        {
            if(OnTargetChange != null)
                OnTargetChange.Invoke(lastTargetData, currTargetData);
            NotifyRaycastEnd(lastTargetData);
            NotifyRaycastStart(currTargetData);
        }
        NotifyRaycastStay(currTargetData);
    }

    public delegate void TargetChangeCallback(TargetData lastTarget, TargetData currentTarget);

    public event TargetChangeCallback OnTargetChange;

    private void NotifyRaycastEnd(TargetData lastTarget)
    {
        RayCastReceiver receiver;
        if (lastTarget.hitting)
        {
            if (lastTarget.targetParent == null) return;
            receiver = lastTarget.targetParent.GetComponent<RayCastReceiver>();
            if (receiver != null)
                receiver.RayCastEnd(this);
        }
    }

    private void NotifyRaycastStart(TargetData newTarget)
    {
        if (!newTarget.hitting) return;
        RayCastReceiver receiver;

        receiver = newTarget.targetParent.GetComponent<RayCastReceiver>();
        if (receiver != null)
            receiver.RayCastStart(this);
    }

    private void NotifyRaycastStay(TargetData currTarget)
    {
        if (!currTarget.hitting) return;
        RayCastReceiver receiver;

        receiver = currTarget.targetParent.GetComponent<RayCastReceiver>();
        if (receiver != null)
            receiver.RayCastStay(this);
    }

    TargetData getTarget()
    {
        bool hitting = false;
        GameObject targetParent;
        GameObject target;
        Vector3 targetPoint;

        RaycastHit hitinfo;
        if (Physics.Raycast(transform.position, transform.forward, out hitinfo, Mathf.Infinity, raycastLayers))
        {
            hitting = true;
            targetParent = hitinfo.transform.gameObject;
            target = hitinfo.collider.gameObject;
            targetPoint = hitinfo.point;
        }
        else
        {
            target = null;
            targetParent = null;
            targetPoint = Vector3.zero;
        }
        this.targetPoint = targetPoint;
        return new TargetData(hitting, target, targetParent);
    }
}
