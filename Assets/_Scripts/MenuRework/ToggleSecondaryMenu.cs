using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ToggleSecondaryMenu : QuickButton
{
    Transform secondaryMenu;
    FuseButton fuseButton;
    Vector3 menuScale, menuLoc;
    Quaternion menuRot;

    protected override void Start()
    {
        base.Start();
        menuScale = new Vector3(1f, 1f, 1f);
        SecondaryMenu secondary = SecondaryMenu.GetInstance();
        if (secondary != null)
        {
            secondaryMenu = secondary.transform.parent;
            fuseButton = this.transform.parent.GetComponentInChildren<FuseButton>();
        }
        else
        {
            Destroy(this.gameObject);
            return;
        }
    }

    void Update()
    {
        menuScale = secondaryMenu.localScale;
        menuLoc = secondaryMenu.localPosition;
        menuRot = secondaryMenu.localRotation;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (secondaryMenu.gameObject.activeSelf)
        {
            secondaryMenu.gameObject.SetActive(false);
        }
        else
        {
            secondaryMenu.gameObject.SetActive(true);
            secondaryMenu.localPosition = menuLoc;
            secondaryMenu.localRotation = menuRot;
            StartCoroutine(ScaleTo(secondaryMenu, secondaryMenu.localScale, menuScale, .3f));
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {
        //fuseButton.ForceCold();
    }

    IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            if (obj == null)
            {
                yield break;
            }
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
    }
}
