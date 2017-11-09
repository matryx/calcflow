using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class ToggleButton : QuickButton {
    public Material onMaterial, offMaterial;
    public string feature;

    bool buttonOn = true;

    Transform backboard, slider;
    Vector3 onPos, offPos;
    Color onColor, offColor;

    protected override void Start ()
    {
        base.Start();

        backboard = transform;
        slider = transform.parent.Find("Slider");
        ColorUtility.TryParseHtmlString("#343836FF", out onColor);
        onColor.a = 1;
        ColorUtility.TryParseHtmlString("#343836FF", out offColor);
        offColor.a = 0;

        onPos = new Vector3(0.21f, slider.localPosition.y, slider.localPosition.z);
        offPos = new Vector3(-0.21f, slider.localPosition.y, slider.localPosition.z);

        if (SettingsVariables.getState(feature))
        {
            setButtonOn();
            slider.localPosition = onPos;
        }
        else
        {
            setButtonOff();
            slider.localPosition = offPos;
        }
    }

    private void setButtonOn()
    {
        backboard.GetComponent<Renderer>().material = onMaterial;

        SettingsVariables.toggleFeature(feature, true);
        buttonOn = true;
    }

    private void setButtonOff()
    {
        backboard.GetComponent<Renderer>().material = offMaterial;

        SettingsVariables.toggleFeature(feature, false);
        buttonOn = false;
    }

    void AnimateButton(bool state)
    {
        IEnumerator move = (state) ? 
                            MoveTo(slider, slider.localPosition, onPos, 0.2f) :
                            MoveTo(slider, slider.localPosition, offPos, 0.2f);

        StartCoroutine(move);
    }

    IEnumerator MoveTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            if (obj == null) break;
            obj.localPosition = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        if (obj != null) obj.localPosition = end;
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        StopAllCoroutines();

        if (buttonOn)
        {
            setButtonOff();
        }
        else
        {
            setButtonOn();
        }

        AnimateButton(buttonOn);
    }

    protected override void ButtonExitBehavior(GameObject other) { }

    void Update () { }
}
