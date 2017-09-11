using UnityEngine;
using System.Collections;

public class Oculus_HandAnim : MonoBehaviour {

    Animator anim;
    int animLayerIndexThumb = -1;
    int animLayerIndexPoint = -1;
    int animParamIndexFlex = -1;
    int animParamIndexPose = -1;
    float animFlex = 0f;
    int animPose = 0;
    bool point = false;
    bool thumbUp = false;
    float animPoint = 0f;
    float animThumbUp = 0f;

	// Use this for initialization
	void Start () {
        anim = GetComponentInChildren<Animator>();

        animLayerIndexPoint = anim.GetLayerIndex("Point Layer");
        animLayerIndexThumb = anim.GetLayerIndex("Thumb Layer");
        animParamIndexFlex = Animator.StringToHash("Flex");
        animParamIndexPose = Animator.StringToHash("Pose");
	}

    public void SetFlex(float f)
    {
        animFlex = f;
    }
    public void SetPoint(bool b)
    {
        point = b;
    }
    public void SetThumb(bool b)
    {
        thumbUp = b;
    }
    public void SetPose(int i)
    {
        animPose = i;
    }
    private float InputValueRateChange(bool x, float value)
    {
        float rateDelta = Time.deltaTime * 20.0f;
        float sign = x ? 1f : -1f;
        return Mathf.Clamp01(value + rateDelta * sign);
    }

    void FixedUpdate()
    {
        animPoint = InputValueRateChange(point, animPoint);
        animThumbUp = InputValueRateChange(thumbUp, animThumbUp);
        anim.SetInteger(animParamIndexPose, animPose);
        anim.SetFloat(animParamIndexFlex, animFlex);
        anim.SetLayerWeight(animLayerIndexPoint, animPoint);
        anim.SetLayerWeight(animLayerIndexThumb, animThumbUp);
    }
}
