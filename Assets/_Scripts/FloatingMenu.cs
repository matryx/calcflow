using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FloatingMenu : MonoBehaviour
{

    bool expanded = false;
    public Vector3 init;
    public Vector3 target;
    RectTransform rect;

    // Use this for initialization
    void Start()
    {
        rect = GetComponent<RectTransform>();
        Vector2 parentSize = transform.parent.GetComponent<RectTransform>().sizeDelta;
        init.x = 0;
        init.y = -rect.sizeDelta.y;
    }

    // Update is called once per frame
    void Update()
    {

    }

    public void Switch()
    {
        expanded = !expanded;
    }

    private void FixedUpdate()
    {
        Vector2 parentSize = transform.parent.GetComponent<RectTransform>().sizeDelta;
        target.x = 0;
        target.y = 0;

        float currRatio = (rect.position - init).magnitude / (target - init).magnitude;
        if (expanded && currRatio < 1)
        {
            rect.position = Vector3.Lerp(init, target, currRatio + 0.05f);
        }
        if (!expanded && currRatio > 0)
        {
            rect.position = Vector3.Lerp(init, target, currRatio - 0.05f);
        }
    }
}
