using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public interface ExpressionTabInterface
{
    ExpressionSet getExpSet();
    Transform getExpressionX();
    Transform getSeparator();
    void setButtonInputColor(Color col);
    void setElementQuadTex(Texture tex);
    void setTextColor(Color c);
    void setExpressionX(Transform e);
    void setSeparator(Transform sep);
    void deleteExpressionFromScroll();
    bool getActiveStatus();
}
